package tui

import logic.*
import logic.MyRandom
import logic.Game
import logic.Stone
import logic.Difficulty
import logic.Stone.*
import logic.Difficulty.*

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

object TUI extends App{

  println()
  println("|---------Konane Game--------|")
  println()

  @tailrec
  def initialMenu(): Game = {
    println("\n[n] Novo Jogo")
    println("[l] Carregar Jogo (savegame.txt)")
    print("Escolha uma opção --> ")

    scala.io.StdIn.readLine().trim.toLowerCase match {
      case "n" => setupNewGame()
      case "l" =>
        FileHandler.loadGame("savegame.txt") match {
          case Some(loadedGame) =>
            println("Jogo carregado com sucesso!")
            loadedGame
          case None =>
            println("Erro: Ficheiro não encontrado ou corrompido.")
            initialMenu()
        }
      case _ => initialMenu()
    }
  }

  //Configuração dum Novo Jogo
  def setupNewGame(): Game = {
    val rows = askSize("linhas")
    val cols = askSize("colunas")
    val pInit = choosePlayer()
    val dif = askDifficulty()

    //Inicialização do Random
    val rand = FileHandler.loadRandom("random.txt") match {
      case Some(value) => value
      case None => MyRandom(System.currentTimeMillis())
    }

    //Criação de Game
    val gameState1 = Game(ParMap.empty[Coord2D, Stone], List.empty[Coord2D], pInit, rand, rows, cols, dif)
    //Inicialização de game
    val newBoardNewList = gameState1.initBoard()
    //Criação de um novo estado
    gameState1.copy(board = newBoardNewList._1, lstOpenCoords = newBoardNewList._2)
  }

  @tailrec
  def askSize(label: String): Int = {
    print(s"Introduza o número de $label: ")
    val input = scala.io.StdIn.readLine()
    input.toIntOption match {
      case Some(n) if n >= 4 => n
      case _ =>
        println("Tamanho inválido! Deve ser um número maior ou igual a 4.")
        askSize(label)
    }
  }

  @tailrec
  def askDifficulty(): Difficulty = {
    print(s"Qual a dificuldade? [e] Fácil;  [m] Médio;  [d] Dificil;  : ")
    val input = scala.io.StdIn.readLine()
    input match {
      case v if v == "e" => Easy
      case v if v == "m" => Medium
      case v if v == "d" => Hard
      case _ => askDifficulty()
    }
  }

  @tailrec
  def askTimer(): Int = {
    println()
    print("Introduza o timer para cada jogada (em segundos): ")
    val input = scala.io.StdIn.readLine()
    println()
    input.toIntOption match {
      case Some(n) if n > 5 => n
      case _ =>
        println("Timer inválido! Tempo de jogada muito curto.")
        askTimer()
    }
  }
  @tailrec
  def choosePlayer(): Stone = {
    println("|-------Choose a piece-------|")
    print("Black(b) | White(w) --> ")
    val input = scala.io.StdIn.readLine()
    input.trim match {
      case value if (value == "b") => Black
      case value if (value == "w") => White
      case _ => choosePlayer()
    }
  }

  val newGameState1 = initialMenu()
  val playerInit = newGameState1.player
  val playerPC = playerInit match {
    case a if(a == White) => Black
    case _ => White
  }
  //Conversao para milisegundos
  val timer = askTimer() * 1000

  //loop do jogo para mudança de estados
  @tailrec
  def mainloop(gameState: Game, pieceCoord: Option[Coord2D], history: List[Game], startTimer: Option[Long]): Unit = {
    val tui = BoarPrinter(gameState)
    //Impressao da board
    tui.printBoard()
    val vez = if (gameState.player == Black) "Pretas (B)" else "Brancas (W)"
    println()
    println(s"|------Vez de jogar: ${vez}------|")
    gameState.player match {
      //Se vez do jogador
      case value if (value == playerInit) => {
        //Definir o que pedir ao jogador
        val prompt = pieceCoord match {
          case Some(pos) => s"CONTINUAÇÃO: Mover a peça de ${deparseCoords(pos)} para onde? (ex: A2)  [f] termina salto;  [r] reiniciar;  "
          case None => "Introduza a jogada (ex: A3 B3)\n[s] guardar;  [u] desfazer;  [r] reiniciar;  "
        }
        // Inicio do timer
        val start = pieceCoord match {
          case Some(_) => startTimer.get
          case None => System.currentTimeMillis()
        }
        print(prompt)

        val input = scala.io.StdIn.readLine().trim.split(" ")

        if (input(0).toLowerCase == "s") {
          FileHandler.saveGame(gameState, "savegame.txt")
          FileHandler.saveRandom(gameState.rand, "random.txt")
          println("Jogo guardado com sucesso!")
          mainloop(gameState, pieceCoord, history, startTimer) // Continua no mesmo estado
        }
        else if(input(0).toLowerCase == "r") {
          mainloop(newGameState1, None, newGameState1 :: Nil, None)
        }
        //Opção de finalizar turno se estiver em salto múltiplo
        else if (input(0).toLowerCase == "f" && pieceCoord.isDefined && startTimer.isDefined) {
          //parar timer
          val total: Long = startTimer match {
            case Some(value) => System.currentTimeMillis() - value
            case None => 0
          }
          if (total > timer) {
            println("|-------Tempo de jogada acabou!-------|")

            mainloop(history.head.copy(player = playerPC), None, history, None)
          }
          else {
            val newGame = gameState.copy(player = playerPC)
            //Adiciona gameState ao historico
            mainloop(newGame, None, newGame :: history, None)
          }
        }
        else if (input(0).toLowerCase == "u") {
          if(history.length < 3) {
            println("|-------Não há mais jogadas para desfazer!-------|")
            mainloop(gameState, pieceCoord, history, startTimer)
          } else {
            //Desfaz jogada anterior do pc e tambem do jogador
            mainloop(history.tail.tail.head, None, history.tail.tail, None)
          }
        }
        else if (input.length < 2 && pieceCoord.isEmpty) {
          println("Erro: Use 'Origem Destino'.")
          mainloop(gameState, pieceCoord, history, startTimer)
        }
        else {
          //Obter coordenadas
          //se pieceCoord existe, from e fixo
          val from = pieceCoord match {
            case Some(value) => value
            case None => parseCoords(input(0))
          }
          val to = pieceCoord match {
            case Some(_) => parseCoords(input(0))
            case None => {
              val timer2 = System.currentTimeMillis()
              parseCoords(input(1))
            }
          }

          gameState.play(from, to) match {
            case (Some(newBoard), newList) =>
              val nextState = gameState.copy(board = newBoard, lstOpenCoords = newList)
              if (nextState.isWinner(playerInit)) {
                println("|--------PARABENS: Voce Ganhou!!---------|")
                FileHandler.saveRandom(gameState.rand, "random.txt")
              }
              else {
                //Se primeira vez que o jogador come a peça, entao ... se noa ...
                if(!pieceCoord.isDefined || !startTimer.isDefined){
                  mainloop(nextState, Some(to), history, Some(start))
                } else {
                  //Adiciona gameState ao historico
                  mainloop(nextState, Some(to), history, startTimer) // Fica no humano, bloqueia a peça
                }
              }
            case _ =>
              println("Jogada impossível!")
              mainloop(gameState, pieceCoord, history, startTimer)
          }
        }
      }
      //Se vez do computador
      case _ => {
        gameState.difficulty match {
          case Easy =>
            val (optNewBoard, newRand, newList, coordTo) = gameState.playRandomly()
            optNewBoard match {
              case Some(newBoard) => {
                val newGameState = gameState.copy(board = newBoard, lstOpenCoords = newList, player = playerInit, rand = newRand)
                println()
                coordTo match {
                  case Some(value) => {
                    println(s"O PC jogou em: ${deparseCoords(value)}")
                    println("----------------------------")
                  }
                  case None =>
                }
                if (newGameState.isWinner(playerPC)) {
                  println("|--------QUE PENA: Voce Perdeu!! (sem jogadas disponiveis)---------|")
                  FileHandler.saveRandom(gameState.rand, "random.txt")
                } else {
                  mainloop(newGameState, None, newGameState :: history, startTimer) //Adiciona gameState ao historico
                }
              }
              case _ =>
                println("|--------PARABENS: Voce Ganhou!!---------|")
                FileHandler.saveRandom(gameState.rand, "random.txt")
            }

          case Medium =>
            val path = gameState.playMediumPath()
            val (newOptBoard, lstOC) = executePath(gameState, path)
            newOptBoard match {
              case Some(value) =>
                val newGameState = gameState.copy(board = value, lstOpenCoords = lstOC, player = playerInit)
                println()
                println(s"O PC jogou em: ${pathToString(path)}")
                println("----------------------------")

                if (newGameState.isWinner(playerPC)) {
                  println("|--------QUE PENA: Voce Perdeu!! (sem jogadas disponiveis)---------|")
                  FileHandler.saveRandom(gameState.rand, "random.txt")
                } else {
                  mainloop(newGameState, None, newGameState :: history, startTimer) //Adiciona gameState ao historico
                }
              case None =>
                println("|--------PARABENS: Voce Ganhou!!---------|")
                FileHandler.saveRandom(gameState.rand, "random.txt")
            }

          case _ =>
            val path = gameState.playHardPath()
            val (newOptBoard, lstOC) = executePath(gameState, path)
            newOptBoard match {
              case Some(value) =>
                val newGameState = gameState.copy(board = value, lstOpenCoords = lstOC, player = playerInit)
                println()
                println(s"O PC jogou em: ${pathToString(path)}")
                println("----------------------------")

                if (newGameState.isWinner(playerPC)) {
                  println("|--------QUE PENA: Voce Perdeu!! (sem jogadas disponiveis)---------|")
                  FileHandler.saveRandom(gameState.rand, "random.txt")
                } else {
                  mainloop(newGameState, None, newGameState :: history, startTimer) //Adiciona gameState ao historico
                }
              case None =>
                println("|--------PARABENS: Voce Ganhou!!---------|")
                FileHandler.saveRandom(gameState.rand, "random.txt")
            }
        }
      }
    }
  }

  @tailrec
  def executePath(gameState: Game, path: List[(Coord2D, Coord2D)]): (Option[Board], List[Coord2D]) = {
    path match {
      case Nil => (Some(gameState.board), gameState.lstOpenCoords)
      case (x, y) :: tail =>
        val newState = gameState.play(x, y)
        newState._1 match {
          case Some(value) =>
            val newGame = gameState.copy(board = value, lstOpenCoords = newState._2)
            println()
            BoardPrinter.printBoard(newGame) //Imprime cada captura
            println()
            executePath(newGame, tail)
          case None => (None, gameState.lstOpenCoords)
        }

    }
  }

  private def pathToString(path: List[(Coord2D, Coord2D)]): String = {
    path match {
      case Nil => ""
      case (firstFrom, _) :: _ =>
        val coords = firstFrom :: path.map(_._2)
        coords.map(deparseCoords).mkString(" -> ")
    }
  }

  private def parseCoords(s: String): Coord2D = {
    val col = s.toUpperCase.head.toInt - 'A'.toInt
    val row = s.tail.toInt
    (row, col)
  }

  private def deparseCoords(c: Coord2D): String = {
    val col = (c._2 + 'A'.toInt).toChar
    s"$col${c._1}"
  }
  mainloop(newGameState1, None, newGameState1 :: Nil, None)
}