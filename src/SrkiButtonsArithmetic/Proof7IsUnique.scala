object Proof7IsUnique extends App {

  // Transition matrices between the four fundamental states (0000, 0001, 0011, 0101)
  // (this is after modding out by inverses and by rotations)
  val s = List(
    List(1, 0, 0, 0),
    List(1, 0, 1, 1),
    List(0, 1, 0, 0),
    List(0, 1, 0, 0))
  val o = List(
    List(1, 0, 0, 0),
    List(0, 1, 0, 0),
    List(0, 0, 1, 0),
    List(1, 0, 0, 0))
  val a = List(
    List(1, 0, 0, 0),
    List(0, 1, 0, 0),
    List(1, 0, 0, 1),
    List(0, 0, 1, 0))

  type Matrix = List[List[Int]]

  // Multiply a vector by a matrix on the right
  def mult(vec: List[Int], mat: Matrix) = (vec, mat).zipped.map((l, rRow) => rRow.map(_*l)).reduce((a, b) => (a, b).zipped.map(_+_))

  // Every possible list of n values from l
  def pow[A](l: List[A], n: Int): List[List[A]] = if (n == 0) List(Nil) else for (a <- l; t <- pow(l, n - 1)) yield a :: t

  // Run this list of operations on the vector corresponding to an unknown but non-winning position
  def execute(ops: List[Matrix]): List[Int] = ops.foldLeft(List(0, 1, 1, 1))(mult)

  val solutions = for {
    ops <- pow(List(s, o, a), 7)
    if (execute(ops) map { _ > 0 }) == List(true, false, false, false) // the only possible final position is the winning one
  } yield ops

  println(solutions == List(List(o, a, o, s, o, a, o)))

}