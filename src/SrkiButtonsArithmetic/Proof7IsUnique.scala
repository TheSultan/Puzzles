object Proof7IsUnique extends App {

  // Transition matrices between the four fundamental states (0000, 0001, 0011, 0101) 
  def s = List(
    List(1, 0, 0, 0),
    List(1, 0, 1, 1),
    List(0, 1, 0, 0),
    List(0, 1, 0, 0))
  def o = List(
    List(1, 0, 0, 0),
    List(0, 1, 0, 0),
    List(0, 0, 1, 0),
    List(1, 0, 0, 0))
  def a = List(
    List(1, 0, 0, 0),
    List(0, 1, 0, 0),
    List(1, 0, 0, 1),
    List(0, 0, 1, 0))

  type Matrix = List[List[Int]]
  def mult(vec: List[Int], mat: Matrix) = (vec, mat).zipped.map((l, rRow) => rRow.map(_*l)).reduce((a, b) => (a, b).zipped.map(_+_))
  def pow[A](l: List[A], n: Int): List[List[A]] = if (n == 0) List(Nil) else for (a <- l; t <- pow(l, n - 1)) yield a :: t
  def execute(ops: List[Matrix]): List[Int] = {
    def loop(vec: List[Int], ops: List[Matrix]): List[Int] = ops match { case Nil => vec; case h :: t => loop(mult(vec, h), t) }
    loop(List(0, 1, 1, 1), ops)
  }
  val solutions = pow(List(s, o, a), 7) filter { x => execute(x) == List(7, 0, 0, 0) }
  println(solutions == List(List(o, a, o, s, o, a, o)))

}
