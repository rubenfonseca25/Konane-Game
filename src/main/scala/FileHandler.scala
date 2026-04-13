import Stone.{Black, White}

import java.io.*
import scala.collection.parallel.immutable.ParMap
import scala.io.Source

object FileHandler {

  def saveGame(gameState: Game, file: String): Unit = {
    val pw = new PrintWriter(new File(file))

    //Dimensoes
    pw.println(s"${gameState.rows},${gameState.cols}")

    //Player
    pw.println(gameState.player.toString)

    //Board
    val boardData = gameState.board.map {
      case ((r, c), stone) => s"$r,$c,$stone"
    }.mkString(";")

    pw.println(boardData)

    pw.close()
    println(s"Jogo guardado em: $file")
  }

  def loadGame(file: String): Option[Game] = {
    try {
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines().toList
      bufferedSource.close()

      //Dimensoes
      val dims = lines(0).split(",")
      val r = dims(0).toInt
      val c = dims(1).toInt

      //Ler Jogador Atual
      val currentPlayer = if (lines(1) == "Black") Stone.Black else Stone.White

      //Reconstruir Board
      //Se o tabuleiro estiver vazio na linha 3, tratamos como Map vazio
      val boardEntries = if (lines(2).isEmpty) Array.empty[String] else lines(2).split(";")

      val boardMap = boardEntries.map { entry =>
        val parts = entry.split(",")
        val coord = (parts(0).toInt, parts(1).toInt)
        val stone = if (parts(2) == "Black") Stone.Black else Stone.White
        coord -> stone
      }.toMap

      //Recalcular lstOpenCoords
      val allCoords = for {
        i <- 0 until r
        j <- 0 until c
      } yield (i, j)

      val openCoords = allCoords.filterNot(boardMap.contains).toList

      //Retorna o Game completo
      Some(Game(boardMap.to(ParMap), openCoords, currentPlayer, r, c))

    } catch {
      case e: Exception =>
        println(s"Erro ao carregar ficheiro: ${e.getMessage}")
        None
    }
  }
}