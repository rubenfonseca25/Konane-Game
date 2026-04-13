import Stone.Black

import scala.annotation.tailrec

case class BoarPrinter(gameState: Game) {
  def printBoard(): Unit = BoardPrinter.printBoard(gameState)
}

object BoardPrinter {
  def printBoard(gameState: Game): Unit = {
    val rows = gameState.rows
    val cols = gameState.cols
    val letra = 'A'
    
    println()
    @tailrec
    def loop(r: Int, c: Int): Unit = {
      (r, c) match {
        case (num1, _) if((num1 == rows + 1)) =>
        case (num1, num2) if(num2 == cols + 1) => println(); loop(num1 + 1, 0)
        case (0, 0) => print("   "); loop(0, 1)
        case (0, num) => print(s" ${(letra + num - 1).toChar}  "); loop(0, num + 1)
        case (num1, 0) => print(f"${num1 - 1}%2d |") ; loop(num1, 1)
        case (num1, num2) => {
          if(gameState.board.contains(num1 - 1, num2 - 1)) {
            if(gameState.board.get(num1 - 1, num2 - 1).contains(Black)) print(" B |")
            else print(" W |")
          }
          else if(gameState.lstOpenCoords.contains(num1 - 1, num2 - 1)) {
            print("   |")
          }
          else print("   |")
          loop(num1, num2 + 1)
        }
      }
    }
    loop(0,0)
    
    println()
    
  }
}