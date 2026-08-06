package tastyquery

/** Representation of the contents of classpaths. */
object Classpaths:
  /** The representation of an entire classpath.
    *
    * Classpaths are made of a sequence of entries (where order is relevant).
    * Each entry contains a set of packages, and packages contain set of class
    * information files.
    */
  type Classpath = List[ClasspathEntry]

  /** One entry of the classpath.
    *
    * A `ClasspathEntry` must have a meaningful `equals` and `hashCode`, which
    * must reflect the identity of the entry (not necessarily the reference
    * identity). Its equality is notably used by
    * [[Contexts.Context.findSymbolsByClasspathEntry]].
    *
    * Users of a `ClasspathEntry` and its components may consider them to be
    * idempotent.
    *
    * All the methods of `ClasspathEntry` and its components may throw
    * `java.io.IOException`s to indicate I/O errors.
    *
    * A `ClasspathEntry` is encouraged to be thread-safe, along with all its
    * components, but it is not a strong requirement. Implementations that are
    * thread-safe should be documented as such. [[Contexts.Context]]s created
    * only from thread-safe `ClasspathEntry`s are thread-safe themselves.
    *
    * Implementations of this class are encouraged to define a `toString()`
    * method that helps identifying the entry for debugging purposes.
    */
  trait ClasspathEntry:
    /** Lists all the packages available in this entry, including nested packages.
      *
      * This method must not return two items with the same [[PackageData.dotSeparatedName]].
      *
      * Subsequent calls to `listAllPackages` may return the same instances of
      * [[PackageData]], but need not do so.
      */
    def listAllPackages(): List[PackageData]
  end ClasspathEntry

  /** Information about one package within a [[ClasspathEntry]].
    *
    * Implementations of this class are encouraged to define a `toString()`
    * method that helps identifying the package and its enclosing classpath
    * entry for debugging purposes.
    */
  trait PackageData:
    /** The fully-qualified name of the package represented by this `PackageData`. */
    val dotSeparatedName: String

    /** Lists all the files containing class information in this package (but not nested packages).
      *
      * Class information is found in `.class` files and `.tasty` files. For
      * any binary name `X`, if there is both an `X.class` and an `X.tasty`,
      * they must be returned as part of the same [[ClassData]].
      *
      * This method must not return two items with the same [[ClassData.binaryName]].
      *
      * Subsequent calls to `listAllClassDatas` and [[getClassDataByBinaryName]]
      * may return the same instances of [[ClassData]], but need not do so.
      */
    def listAllClassDatas(): List[ClassData]

    /** Get the [[ClassData]] associated with the given `binaryName` in this package, if it exists.
      *
      * Returns `None` if neither `binaryName.class` nor `binaryName.tasty` exists.
      *
      * Subsequent calls to `getClassDataByBinaryName` and [[listAllClassDatas]]
      * may return the same instance of [[ClassData]], but need not do so.
      */
    def getClassDataByBinaryName(binaryName: String): Option[ClassData]
  end PackageData

  /** Information about one class within a [[PackageData]].
    *
    * When both a `.class` file and a `.tasty` file exist for a given binary
    * name, they are represented by the same instance of `ClassData`.
    *
    * Implementations of this class are encouraged to define a `toString()`
    * method that helps identifying the class and its enclosing package and
    * classpath entry for debugging purposes.
    */
  trait ClassData:
    /** The binary name of the class information represented by this `ClassData`.
      *
      * It is the name of the file(s) without the `.class` or `.tasty` extension.
      */
    val binaryName: String

    /** Tests whether this class information has an associated `.tasty` file. */
    def hasTastyFile: Boolean

    /** Reads the contents of the `.tasty` file associated with this class information. */
    def readTastyFileBytes(): IArray[Byte]

    /** Tests whether this class information has an associated `.class` file. */
    def hasClassFile: Boolean

    /** Reads the contents of the `.class` file associated with this class information. */
    def readClassFileBytes(): IArray[Byte]
  end ClassData

  /** An abstraction over an individual file on the classpath (virtual file).
    *
    * This augments [[ClasspathFile]] with a [[readNow]] method that can be used during the initial indexing of
    * the classpath to reuse open file descriptors.
    *
    * This trait doesn't extend [[ClasspathFile]] because we don't want [[ClasspathFile]]s read later to hold onto
    * references to open files.
    */
  private[tastyquery] trait OpenClasspathFile:
    def classpathFile: ClasspathFile

    /** Read the contents of the file during the initial indexing of the classpath.
      *
      * This should be preferred over [[ClasspathFile.read]] during indexing, as it reuses open file descriptors. If
      * called later, an exception will be thrown.
      */
    def readNow(): IArray[Byte]
  end OpenClasspathFile

  /** An abstraction over an individual file on the classpath (virtual file). */
  private[tastyquery] trait ClasspathFile:
    /** Read the contents of the file.
      *
      * Implementations of this method should always open the file and not reference any open file descriptors, as this
      * may be called after the initial indexing of the classpath.
      */
    def read(): IArray[Byte]
  end ClasspathFile

  /** An abstraction over a representation of the classpath.
    *
    * Currently, [[InMemory]] and [[OnDisk]] are the only implementations.
    */
  private[tastyquery] trait ClasspathRepresentation:
    type ClasspathEntry <: Classpaths.ClasspathEntry
    type PackageData <: Classpaths.PackageData
    type ClassData <: Classpaths.ClassData

    def classData(
      debugString: String,
      binaryName: String,
      tastyFile: Option[OpenClasspathFile],
      classFile: Option[OpenClasspathFile]
    ): ClassData

    def classpathEntry(debugString: String, packages: List[PackageData]): ClasspathEntry
    def combineClassData(classData1: ClassData, classData2: ClassData): ClassData
    def packageData(debugString: String, dotSeparatedName: String, classes: List[ClassData]): PackageData
  end ClasspathRepresentation

  /** In-memory representation of classpath entries.
    *
    * In-memory classpath entries are thread-safe.
    */
  object InMemory extends ClasspathRepresentation:
    import Classpaths as generic

    /** A thread-safe, immutable classpath entry. */
    final class ClasspathEntry(debugString: String, val packages: List[PackageData]) extends generic.ClasspathEntry:
      override def toString(): String = debugString

      def listAllPackages(): List[generic.PackageData] = packages
    end ClasspathEntry

    /** A thread-safe, immutable package information within a classpath entry. */
    final class PackageData(debugString: String, val dotSeparatedName: String, val classes: List[ClassData])
        extends generic.PackageData:
      private lazy val byBinaryName = classes.view.map(c => c.binaryName -> c).toMap

      override def toString(): String = debugString

      def listAllClassDatas(): List[generic.ClassData] = classes

      def getClassDataByBinaryName(binaryName: String): Option[generic.ClassData] = byBinaryName.get(binaryName)
    end PackageData

    /** A thread-safe, immutable class information within a classpath entry. */
    final class ClassData(
      private[InMemory] val debugString: String,
      val binaryName: String,
      val tastyFileBytes: Option[IArray[Byte]],
      val classFileBytes: Option[IArray[Byte]]
    ) extends generic.ClassData:
      override def toString(): String = debugString

      def hasTastyFile: Boolean = tastyFileBytes.isDefined

      def readTastyFileBytes(): IArray[Byte] =
        tastyFileBytes.getOrElse(throw new Exception(s"${this} has no TASTy file."))

      def hasClassFile: Boolean = classFileBytes.isDefined

      def readClassFileBytes(): IArray[Byte] =
        classFileBytes.getOrElse(throw new Exception(s"${this} has no class file."))

      def combineWith(that: ClassData): ClassData = InMemory.combineClassData(this, that)
    end ClassData

    override def classData(
      debugString: String,
      binaryName: String,
      tastyFile: Option[OpenClasspathFile],
      classFile: Option[OpenClasspathFile]
    ): ClassData = new ClassData(debugString, binaryName, tastyFile.map(_.readNow()), classFile.map(_.readNow()))

    override def classpathEntry(debugString: String, packages: List[PackageData]): ClasspathEntry =
      new ClasspathEntry(debugString, packages)

    override def combineClassData(classData1: ClassData, classData2: ClassData): ClassData =
      require(
        classData1.binaryName == classData2.binaryName,
        s"cannot combine two ClassData for different binary names ${classData1.binaryName} and ${classData2.binaryName}"
      )

      ClassData(
        classData1.debugString,
        classData1.binaryName,
        classData1.tastyFileBytes.orElse(classData2.tastyFileBytes),
        classData1.classFileBytes.orElse(classData2.classFileBytes)
      )

    override def packageData(debugString: String, dotSeparatedName: String, classes: List[ClassData]): PackageData =
      new PackageData(debugString, dotSeparatedName, classes)
  end InMemory

  /** Lazily loaded, on-disk representation of classpath entries.
    *
    * Unlike [[InMemory]], [[OnDisk]] loads `.tasty` and `.class` files lazily, when they're needed. This may be desired
    * when eagerly loading every file on the classpath would take too much time or memory.
    *
    * [[OnDisk]] doesn't cache file content, so repeated calls to [[ClassData.readTastyFileBytes]] or
    * [[ClassData.readClassFileBytes]] will read the file from disk each time. This shouldn't be a problem in practice,
    * as [[Contexts.Context]] caches the deserialized representation of these files. Note that this means the files need
    * to be readable for the lifetime of the [[Contexts.Context]] that uses them.
    */
  object OnDisk extends ClasspathRepresentation:
    import Classpaths as generic

    final class ClasspathEntry(debugString: String, val packages: List[PackageData]) extends generic.ClasspathEntry:
      override def listAllPackages(): List[generic.PackageData] = packages
      override def toString: String = debugString
    end ClasspathEntry

    final class PackageData(debugString: String, val dotSeparatedName: String, val classes: List[ClassData])
        extends generic.PackageData:
      private lazy val byBinaryName = classes.view.map(`class` => `class`.binaryName -> `class`).toMap

      override def getClassDataByBinaryName(binaryName: String): Option[ClassData] = byBinaryName.get(binaryName)
      override def listAllClassDatas(): List[ClassData] = classes
      override def toString: String = debugString
    end PackageData

    final class ClassData(
      private[OnDisk] val debugString: String,
      val binaryName: String,
      val tastyFile: Option[ClasspathFile],
      val classFile: Option[ClasspathFile]
    ) extends generic.ClassData:
      override def hasTastyFile: Boolean = tastyFile.isDefined
      override def readTastyFileBytes(): IArray[Byte] =
        tastyFile.getOrElse(throw new Exception(s"${this} has no TASTy file.")).read()

      override def hasClassFile: Boolean = classFile.isDefined
      override def readClassFileBytes(): IArray[Byte] =
        classFile.getOrElse(throw new Exception(s"${this} has no class file.")).read()

      override def toString: String = debugString
    end ClassData

    override def classData(
      debugString: String,
      binaryName: String,
      tastyFile: Option[OpenClasspathFile],
      classFile: Option[OpenClasspathFile]
    ): ClassData =
      new ClassData(debugString, binaryName, tastyFile.map(_.classpathFile), classFile.map(_.classpathFile))

    override def classpathEntry(debugString: String, packages: List[PackageData]): ClasspathEntry =
      new ClasspathEntry(debugString, packages)

    override def combineClassData(classData1: ClassData, classData2: ClassData): ClassData =
      require(
        classData1.binaryName == classData2.binaryName,
        s"cannot combine two ClassData for different binary names ${classData1.binaryName} and ${classData2.binaryName}"
      )

      ClassData(
        classData1.debugString,
        classData1.binaryName,
        classData1.tastyFile.orElse(classData2.tastyFile),
        classData1.classFile.orElse(classData2.classFile)
      )

    override def packageData(debugString: String, dotSeparatedName: String, classes: List[ClassData]): PackageData =
      new PackageData(debugString, dotSeparatedName, classes)
  end OnDisk
end Classpaths
