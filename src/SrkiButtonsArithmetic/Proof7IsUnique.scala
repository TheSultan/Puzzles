object Proof7IsUnique extends App {

  // Transition matrices between the four fundamental states (0000, 0001, 0011, 0101)
  // (this is after modding out by inverses and by rotations)
  case class Matrix(name: String, transitions: List[Double]*) {
    override def toString = name
    override def equals(any: Any) = eq(any.asInstanceOf[AnyRef])
  }

  val s = Matrix("Single",
    List(1,     0, 0,   0),
    List(0.25,  0, 0.5, 0.25), // 1/4 chance of hitting exactly, 1/2 chance of landing adjacent, 1/4 chance of landing opposite
    List(0,     1, 0,   0),
    List(0,     1, 0,   0))
  val o = Matrix("Opposite",
    List(1, 0, 0, 0),
    List(0, 1, 0, 0),
    List(0, 0, 1, 0),
    List(1, 0, 0, 0))
  val a = Matrix("Adjacent",
    List(1,   0, 0, 0),
    List(0,   1, 0, 0),
    List(0.5, 0, 0, 0.5), // 1/2 chance of hitting exactly, 1/2 chance of overlapping on either edge
    List(0,   0, 1, 0))

  val initialProbabilities = List(0.0, 8.0/14, 4.0/14, 2.0/14)
  // 8.0 = 4 choices with one button + 4 choices with three buttons
  // 4.0 = |{ 0011, 0110, 1100, 1001 }|
  // 2.0 = |{ 0101, 1010 }|
  // 14 = 2^4 - |{ 0000, 1111 }|

  // Multiply a vector by a matrix on the right
  def mult(vec: List[Double], mat: Matrix) = (vec, mat.transitions).zipped.map((l, rRow) => rRow.map(_*l)).reduce((a, b) => (a, b).zipped.map(_+_))

  // Every possible list of n values from l
  def pow[A](l: List[A], n: Int): List[List[A]] = if (n == 0) List(Nil) else for (a <- l; t <- pow(l, n - 1)) yield a :: t

  def proof7IsUnique = {
    val solutions = for {
      ops <- pow(List(s, o, a), 7)
      if (ops.foldLeft(initialProbabilities)(mult) map { _ > 0 }) == List(true, false, false, false) // the only possible final position is the winning one
    } yield ops

    println(solutions)
    println(solutions == List(List(o, a, o, s, o, a, o)))
  }

  def findMinimumEV = {
    val solutions = for {
      size <- 7 to 10
      ops <- pow(List(s, o, a), size)
      probs = ops.scanLeft(initialProbabilities)(mult) map { _.head }
      if (probs.last > 0.999)
      val (diffs, _) = probs.foldRight((Nil: List[Double], 1.0)) { (prob, pair) =>
        val (l, prev) = pair
        ((prev - prob) :: l, prob)
      }
      ev = diffs.zipWithIndex map { case (d, i) => d * (i + 1) } sum
    } yield (ops, diffs, ev)
    for (s <- solutions.sortBy(_._3)) {
      println(s)
    }
  }

  proof7IsUnique
  findMinimumEV
}