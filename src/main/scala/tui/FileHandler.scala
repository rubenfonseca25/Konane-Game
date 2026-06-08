package tui

import logic.*
import logic.Game
import logic.Stone
import logic.Difficulty
import logic.Stone.{Black, White}
import logic.Difficulty.{Easy, Medium, Hard}

import java.io.*
import scala.collection.parallel.immutable.ParMap
import scala.io.Source

object FileHandler {

  def saveRandom(rand: MyRandom, file: String): Unit = {
    val pw = new PrintWriter(new File(file))
    pw.println(rand.seed)
    pw.close()
  }
  
  def loadRandom(file: String): Option[MyRandom] = {
    try {
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines().toList
      bufferedSource.close()
      
      val seed = lines.head.toLong
      Some(MyRandom(seed))
      
    } catch {
      case e: Exception =>
        println(s"Erro ao carregar ficheiro: ${e.getMessage}")
        None
    }
  }
  
  def saveGame(gameState: Game, file: String): Unit = {
    val pw = new PrintWriter(new File(file))

    //Dimensoes
    pw.println(s"${gameState.rows},${gameState.cols}")

    //Player
    pw.println(gameState.player.toString)
    
    //Dificulty
    pw.println(gameState.difficulty.toString)
    
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

      //enum dificuldade
      val difficulty = if (lines(2) == "Easy") Easy else if (lines(2) == "Medium") Medium else Hard
      
      //Reconstruir Board
      //Se o tabuleiro estiver vazio na linha 3, tratamos como Map vazio
      val boardEntries = if (lines(3).isEmpty) Array.empty[String] else lines(3).split(";")

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
      loadRandom("random.txt") match {
        case Some(value) => Some(Game(boardMap.to(ParMap), openCoords, currentPlayer, value, r, c, difficulty))
        case None => {
          println("Erro ao carregar ficheiro (random)")
          None
        }
      }


    } catch {
      case e: Exception =>
        println(s"Erro ao carregar ficheiro: ${e.getMessage}")
        None
    }
  }
}