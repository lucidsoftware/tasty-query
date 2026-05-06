package simple_trees

class InferredTypeArgs {
  def id[T](x: T): T = x

  val a = id(42)
}
