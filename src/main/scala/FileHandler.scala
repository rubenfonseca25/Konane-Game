import Stone.{Black, White}

import java.io.*
import scala.collection.parallel.immutable.ParMap
import scala.io.Source

object FileHandler {

  def saveGame(file: String, gameState: Game): Unit = {
    val pw = new PrintWriter(new File(file))

    // 1. Metadados: Linhas, Colunas, Jogador Atual
    pw.println(s"META:${gameState.rows}:${gameState.cols}:${gameState.player}")

    // 2. Gravar o Board (Peças) usando o ciclo for permitido
    for {
      r <- 0 until gameState.rows
      c <- 0 until gameState.cols
    } {
      gameState.board.get((r, c)).foreach { piece =>
        pw.println(s"PIECE:$r:$c:$piece")
      }
    }

    // 3. Gravar OpenCoords
    gameState.lstOpenCoords.foreach { case (r, c) =>
      pw.println(s"OPEN:$r:$c")
    }

    pw.close()
  }

  def loadGame(file: String): Game = {
    val lines = Source.fromFile(file).getLines().toList

    // Variáveis temporárias para reconstruir o estado
    var rows = 0
    var cols = 0
    var player: Stone = Black
    var board = Map[Coord2D, Stone]()
    var openCoords = List[Coord2D]()

    // Ciclo for para processar as linhas (conforme permitido pelo guião)
    for (line <- lines) {
      val parts = line.split(":")
      parts(0) match {
        case "META" =>
          rows = parts(1).toInt
          cols = parts(2).toInt
          player = if (parts(3) == "Black") Black else White

        case "PIECE" =>
          val r = parts(1).toInt
          val c = parts(2).toInt
          val p = if (parts(3) == "Black") Black else White
          board += ((r, c) -> p)

        case "OPEN" =>
          val r = parts(1).toInt
          val c = parts(2).toInt
          openCoords = (r, c) :: openCoords

        case _ => // Ignora linhas vazias ou estranhas
      }
    }

    val boardPar = ParMap(board.toSeq: _*)
    
    Game(boardPar, openCoords, player, rows, cols)
  }
}