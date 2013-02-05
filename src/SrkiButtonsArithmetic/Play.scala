import scala.collection._

/*
  Puzzle courtesy of Srdjan
  Original source (spoiler / solution alert): http://arcsecond.wordpress.com/2013/01/21/spinning-room-puzzle/
 */
class Board(buttons: mutable.Buffer[Boolean]) {

  def hasWon() = buttons.forall(!_) || buttons.forall(b => b)

  def flipOne() = { buttons(0) = !buttons(0) }
  def flipDiagonal() = { buttons(1) = !buttons(1); buttons(2) = !buttons(2) }
  def flipVertical() = { buttons(0) = !buttons(0); buttons(2) = !buttons(2) }

  private def s(i: Int) = if (buttons(i)) "X" else "0"

  /*
   0 1
   2 3
   */
  override def toString = "\t" + s(0) + s(1) + "\n\t" + s(2) + s(3) + "\n"
}


for (i <- 0 to 15) {
  val buttons = for (j <- 0 to 3) yield if ((i & (math.pow(2, j).toInt)) != 0) true else false
  val board = new Board(buttons.toBuffer)
  var moves = 0
  var won = false

  def printBoard(force: Boolean = false) = {
    if (force) println(board.toString)
  }

  def hasWon() : Boolean = {
    if (board.hasWon()) {
      if (!won) {
        println("Won in " + moves + " moves")
        won = true
      }
      return true
    }
    return false
  }

  def makeMoveUnlessWon(move: () => Unit) = {
    if (!hasWon()) {
      move()
      moves += 1
      printBoard()
    }
  }

  println("Configuration " + i)
  //printBoard(true)

  def solveEvens() = {
    makeMoveUnlessWon(board.flipDiagonal _)
    makeMoveUnlessWon(board.flipVertical _)
    makeMoveUnlessWon(board.flipDiagonal _)
  }

  solveEvens()
  makeMoveUnlessWon(board.flipOne _)
  solveEvens()
  if (!hasWon()) println("FAIL!!!")
}