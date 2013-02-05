import scala.collection._

/*
   0 1
   2 3
 */
class Board(buttons: mutable.Buffer[Boolean]) {

  def hasWon() = buttons.forall(!_) || buttons.forall(b => b)

  def flipOne() = { buttons(0) = !buttons(0) }
  def flipDiagonal() = { buttons(1) = !buttons(1); buttons(2) = !buttons(2) }
  def flipVertical() = { buttons(0) = !buttons(0); buttons(2) = !buttons(2) }

  private def s(i: Int) = if (buttons(i)) "X" else "0"


  override def toString = "\t" + s(0) + s(1) + "\n\t" + s(2) + s(3) + "\n"
}


for (i <- 0 to 15
  //filter(_ == 3)
) {
  val buttons = for (j <- 0 to 3) yield if ((i & (math.pow(2, j).toInt)) != 0) true else false
  val board = new Board(buttons.toBuffer)

  def printBoard(force: Boolean = false) = {
    if (force) println(board.toString)
  }

  println("Configuration " + i)
  printBoard(true)

  def solveEvens(iteration: Int = 1) = {
    def makeMoveUnlessWon(move: () => Unit) = {
      if (board.hasWon())
        println("Won in " + iteration)
      else {
        move()
        printBoard()
      }
    }

    makeMoveUnlessWon(board.flipDiagonal _)

    if (board.hasWon()) {
      println("Won in " + iteration)
    } else {
      board.flipDiagonal()
      printBoard()
      if (board.hasWon()) {
        println("Won in " + (iteration + 1) )
      } else {
        board.flipVertical()
        printBoard()
        if (board.hasWon()) {
          println("Won in " (iteration + 2))
        } else {
          board.flipDiagonal()
          printBoard()
          if (board.hasWon()) {
            println("Won in " + (iteration + 3))
          }
        }
      }
    }
  }

  if (board.hasWon()) {
    println("Won in 0")
  } else {
    solveEvens()
    if (board.hasWon()) {
      println("Won in 3")
    } else {
      board.flipOne()
      if (board.hasWon()) {
        println("Won in 4")
      } else {
        printBoard()
        solveEvens(4)
        if (!board.hasWon()) {
          println("FAIL!!!")
          printBoard()
        }
      }
    }
  }
}