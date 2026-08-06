package tastyquery.jdk

import java.io.{IOException, InputStream}
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar.{JarEntry, JarFile}
import scala.collection.mutable
import scala.util.Using
import tastyquery.Classpaths.*

/** Classpath loaders using the JDK API.
  *
  * This API is specific to the JVM. It is not available in Scala.js.
  */
object ClasspathLoaders {

  private enum FileKind(val ext: String):
    case Class extends FileKind("class")
    case Tasty extends FileKind("tasty")

    private val suffix = "." + ext

    def appliesTo(path: Path): Boolean = appliesTo(path.getFileName.nn.toString)
    def appliesTo(filename: String): Boolean = filename.endsWith(suffix)
    def removeExtension(fileName: String): String =
      fileName.substring(0, fileName.length() - suffix.length())
  end FileKind

  private object FileKind:
    lazy val All: Set[FileKind] = Set(Class, Tasty)
  end FileKind

  /** Reads the contents of a classpath.
    *
    * Entries can be directories or jar files. Non-existing entries are
    * ignored.
    *
    * This method will synchronously read the contents of all `.class` and `.tasty` files on the classpath. However, if
    * [[lazyLoading]] is set to `true`, these files won't be read until they're needed. Indexing will still be performed
    * eagerly so we know _which_ `.class` and `.tasty` files to read.
    *
    * The resulting [[Classpaths.Classpath]] can be given to [[Contexts.Context.initialize]]
    * to create a [[Contexts.Context]]. The latter gives semantic access to all
    * the definitions on the classpath.
    *
    * The entries of the resulting [[Classpaths.Classpath]] are all guaranteed
    * to be thread-safe.
    *
    * @param classpath The list of classpath entries to read.
    * @param lazyLoading If set to `true`, the `.class and `.tasty` files won't be read until they're needed.
    *
    * @note the resulting [[Classpaths.ClasspathEntry ClasspathEntry]] entries of
    *       the returned [[Classpaths.Classpath]] correspond to the elements of `classpath`.
    */
  def read(classpath: List[Path], lazyLoading: Boolean): Classpath =
    read(if (lazyLoading) OnDisk else InMemory)(classpath, FileKind.All, lazyLoading)

  def read(classpath: List[Path]): Classpath = read(classpath, lazyLoading = false)

  private def read(
    representation: ClasspathRepresentation
  )(classpath: List[Path], kinds: Set[FileKind], lazyLoading: Boolean): Classpath =
    val representation = if (lazyLoading) OnDisk else InMemory

    def classAndPackage(binaryName: String): (String, String) = {
      val lastSep = binaryName.lastIndexOf('.')
      if (lastSep == -1) ("", binaryName)
      else
        import scala.language.unsafeNulls
        val packageName = binaryName.substring(0, lastSep)
        val simpleName = binaryName.substring(lastSep + 1)
        (packageName, simpleName)
    }

    def binaryName(classFile: String): String =
      /* Replace *both* slashes and backslashes, because the Java file APIs
       * are permissive in what they manipulate, so it's possible to get,
       * especially on Windows.
       */
      classFile.replace('/', '.').nn.replace('\\', '.').nn
    end binaryName

    def compressPackageData(
      entryDebugString: String,
      data: List[(String, representation.ClassData)]
    ): List[representation.PackageData] =
      val groupedPackages = data.groupMap((pkg, _) => pkg)((_, data) => data)
      groupedPackages.map { (packageName, allClassDatas) =>
        val packageDebugString = entryDebugString + ":" + packageName
        val mergedClassDatas =
          allClassDatas.groupMapReduce(_.binaryName)(identity)(representation.combineClassData).valuesIterator.toList
        representation.packageData(packageDebugString, packageName, mergedClassDatas)
      }.toList
    end compressPackageData

    def toEntry(entryDebugString: String, entry: ClasspathEntryKind): representation.ClasspathEntry =
      val map = entry.walkFiles(kinds.toSeq*) { (kind, fileWithExt, path, classpathFile) =>
        val file = kind.removeExtension(fileWithExt)
        val bin = binaryName(file)
        val (packageName, simpleName) = classAndPackage(bin)
        kind match {
          case FileKind.Class =>
            packageName -> representation.classData(path, simpleName, None, Some(classpathFile))
          case FileKind.Tasty =>
            packageName -> representation.classData(path, simpleName, Some(classpathFile), None)
        }
      }
      val packageDatas = compressPackageData(
        entryDebugString,
        map.get(FileKind.Class).getOrElse(Nil) ++ map.get(FileKind.Tasty).getOrElse(Nil)
      )
      representation.classpathEntry(entryDebugString, packageDatas)
    end toEntry

    classpathToEntries(classpath).map(toEntry)
  end read

  private def loadBytes(fileStream: InputStream): IArray[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    while
      val read = fileStream.read(buffer)
      if read >= 0 then bytes.write(buffer, 0, read)
      read >= 0
    do ()
    IArray.from(bytes.toByteArray().nn)
  }

  private def classpathToEntries(classpath: List[Path]): List[(String, ClasspathEntryKind)] =
    for e <- classpath yield
      val entryKind =
        if Files.exists(e) then
          if Files.isDirectory(e) then ClasspathEntryKind.Directory(e)
          else if e.getFileName().toString().endsWith(".jar") then ClasspathEntryKind.Jar(e)
          else throw IllegalArgumentException("Illegal classpath entry: " + e)
        else ClasspathEntryKind.Empty
      e.toString() -> entryKind
  end classpathToEntries

  private enum ClasspathEntryKind {
    case Jar(path: Path)
    case Directory(path: Path)
    case Empty

    def walkFiles[T](kinds: FileKind*)(op: (FileKind, String, String, OpenClasspathFile) => T): Map[FileKind, List[T]] =
      this match {
        case Jar(path) =>
          def getFullPath(filename: String): String = s"$path:$filename"
          val matching = mutable.HashMap.from(kinds.map(kind => kind -> mutable.ListBuffer.empty[JarEntry]))
          Using(JarFile(path.toFile())) { jar =>
            val results = {
              import scala.language.unsafeNulls
              val stream = jar.stream
              stream.forEach { je =>
                val entryName = je.getName
                kinds.find(_.appliesTo(entryName)).foreach(matching(_) += je)
              }
              matching.map { case kind -> jes =>
                kind ->
                  jes.toList.map { je =>
                    val entryName = je.getName
                    val _classpathFile = new ClasspathFile {
                      override def read(): IArray[Byte] =
                        Using(JarFile(path.toFile)) { jar =>
                          val je = Option(jar.getJarEntry(entryName)).getOrElse(
                            throw new Exception(s"`$entryName` no longer exists in `$path`.")
                          )

                          Using(jar.getInputStream(je))(loadBytes).get
                        }.get
                    }

                    val openClasspathFile = new OpenClasspathFile {
                      override def classpathFile: ClasspathFile = _classpathFile
                      override def readNow(): IArray[Byte] = Using(jar.getInputStream(je))(loadBytes).get
                    }

                    op(kind, je.getName(), getFullPath(je.getName()), openClasspathFile)
                  }
              }.toMap
            }
            results
          }.get

        case Directory(path) =>
          import scala.language.unsafeNulls

          val matching = mutable.HashMap.from(kinds.map(kind => kind -> mutable.ListBuffer.empty[Path]))

          Files.walkFileTree(
            path,
            new FileVisitor[Path] {
              def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
                FileVisitResult.CONTINUE

              def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
                for applicableKind <- kinds.find(_.appliesTo(file)) do matching(applicableKind) += file
                FileVisitResult.CONTINUE

              def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
                FileVisitResult.CONTINUE

              def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
                FileVisitResult.CONTINUE
            }
          )

          matching.map { case ext -> files =>
            ext ->
              files.toList.map { f =>
                val _classpathFile = new ClasspathFile {
                  override def read(): IArray[Byte] = IArray.from(Files.readAllBytes(f))
                }

                val openClasspathFile = new OpenClasspathFile {
                  override def classpathFile: ClasspathFile = _classpathFile
                  override def readNow(): IArray[Byte] = IArray.from(Files.readAllBytes(f))
                }

                op(ext, path.relativize(f).toString(), f.toString(), openClasspathFile)
              }
          }.toMap

        case Empty => Map.empty
      }
  }

}
