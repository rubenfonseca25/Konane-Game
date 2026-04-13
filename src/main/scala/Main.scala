import Stone.Black
import Stone.White

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

object Main extends App{

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

    //Criação de Game
    val gameState1 = Game(ParMap.empty[Coord2D, Stone], List.empty[Coord2D], Stone.White, rows, cols)
    //Inicialização de game
    val newBoardNewList = gameState1.initBoard()
    //Criação de um novo estado
    gameState1.copy(board = newBoardNewList._1, lstOpenCoords = newBoardNewList._2)
  }

  @tailrec
  def askSize(label: String): Int = {
    println(s"Introduza o número de $label:")
    val input = scala.io.StdIn.readLine()
    input.toIntOption match {
      case Some(n) if n >= 4 => n
      case _ =>
        println("Tamanho inválido! Deve ser um número maior ou igual a 4.")
        askSize(label)
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

  val rand = MyRandom(System.currentTimeMillis())

  //loop do jogo para mudança de estados
  @tailrec
  def mainloop(gameState: Game, r: MyRandom, pieceCoord: Option[Coord2D] = None): Unit = {
    val tui = BoarPrinter(gameState)
    //Impressao da board
    tui.printBoard()
    val vez = if (gameState.player == Black) "Pretas (B)" else "Brancas (W)"
    println()
    println(s"|------Vez de jogar: ${vez}------|")
    gameState.player match {
      //Se vez do jogador
      case value if(value == playerInit) => {
        //Definir o que pedir ao jogador
        val prompt = pieceCoord match {
          case Some(pos) => s"CONTINUAÇÃO: Mova a peça de ${deparseCoords(pos)} para onde? (ou 'f' para terminar): "
          case None => "Introduza a jogada (ex: A3 B3) ou 's' para guardar: "
        }
        print(prompt)
        val input = scala.io.StdIn.readLine().trim.split(" ")

        if (input(0).toLowerCase == "s") {
          FileHandler.saveGame(gameState, "savegame.txt")
          println("Jogo guardado com sucesso!")
          mainloop(gameState, r, pieceCoord) // Continua no mesmo estado
        }
        //Opção de finalizar turno se estiver em salto múltiplo
        else if (input(0).toLowerCase == "f" && pieceCoord.isDefined) {
          mainloop(gameState.copy(player = playerPC), r, None)
        }
        else if (input.length < 2 && pieceCoord.isEmpty) {
          println("Erro: Use 'Origem Destino'.")
          mainloop(gameState, r, pieceCoord)
        }
        else {
          //Obter coordenadas
          //se pieceCoord existe, from é fixo)
          val from = pieceCoord.getOrElse(parseCoords(input(0)))
          val to = if (pieceCoord.isDefined) parseCoords(input(0)) else parseCoords(input(1))

          gameState.play(from, to) match {
            case (Some(newBoard), newList) =>
              // 4. Verificar se pode continuar a saltar com a MESMA peça
              if (gameState.isValidMove(from, to)) {
                println("Podes continuar a saltar!")
                val nextState = gameState.copy(board = newBoard, lstOpenCoords = newList)
                mainloop(nextState, r, Some(to)) // Fica no humano, bloqueia a peça
              } else {
                val nextState = gameState.copy(board = newBoard, lstOpenCoords = newList, player = playerPC)
                mainloop(nextState, r, None) // Passa para o PC
              }
            case _ =>
              println("Jogada impossível!")
              mainloop(gameState, r, pieceCoord)
          }
        }
      }
      //Se vez do computador
      case _ => {
        val (optNewBoard, newRand, newList, coordTo) = gameState.playRandomly(r)
        optNewBoard match {
          case Some(newBoard) => {
            val newGameState = gameState.copy(board = newBoard, lstOpenCoords = newList, player = playerInit)
            println()
            coordTo match {
              case Some(value) => {
                println(s"O PC jogou em: ${deparseCoords(value)}")
                println("----------------------------")

              }
              case None =>
            }
            mainloop(newGameState, newRand)
          }
          case _ =>
            println(s"Fim do jogo! O jogador ${gameState.player} não tem mais movimentos.")
            println("Obrigado por jogar!")
        }
      }
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
  mainloop(newGameState1, rand)
}