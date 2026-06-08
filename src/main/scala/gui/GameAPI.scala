package gui

import logic.*
import logic.Game
import logic.Difficulty
import logic.Difficulty.*
import logic.Stone
import logic.Stone.*
import logic.Coord2D

class GameAPI(rows: Int, cols: Int, difficultyInt: Int) {

  // Conversores utilitários entre Int da GUI e o teu Enum Difficulty
  private def intToDifficulty(d: Int): Difficulty = d match {
    case 1 => Easy
    case 2 => Medium
    case 3 => Hard
    case _ => Easy
  }

  private def difficultyToInt(d: Difficulty): Int = d match {
    case Easy   => 1
    case Medium => 2
    case Hard   => 3
  }

  // Inicializa o estado de MyRandom com a semente de tempo atual
  private var rand = MyRandom(System.currentTimeMillis())

  private val (initialBoard, initialOpen) = Game.initBoard(rows, cols)
  private var game = Game(initialBoard, initialOpen, Black, rand, rows, cols, intToDifficulty(difficultyInt))

  private var selected: Option[Coord2D] = None
  private var multiJumpPiece: Option[Coord2D] = None
  private var history: List[Game] = Nil

  def getRows: Int = game.rows
  def getCols: Int = game.cols
  def getDifficulty: Int = difficultyToInt(game.difficulty)

  def getStone(row: Int, col: Int): String = {
    game.board.get((row, col)) match {
      case Some(Black) => "BLACK"
      case Some(White) => "WHITE"
      case None        => "EMPTY"
    }
  }

  def isSelected(row: Int, col: Int): Boolean = {
    selected.contains((row, col)) || multiJumpPiece.contains((row, col))
  }

  def currentPlayer: String = if (game.player == Black) "BLACK" else "WHITE"

  def nextPlayer(player: Stone): Stone = if (player == Black) White else Black

  def isGameOver: Boolean = {
    if (game.player == Black && multiJumpPiece.isDefined) false
    else Game.validMoves(game.lstOpenCoords, game.player, game.board).isEmpty
  }

  def jogarAleatorioHumano(): Unit = {
    if (game.player != Black || isGameOver) {
      return
    }
    history = game :: history

    val (optBoard, novoRand, novaLista, _) = game.playRandomly()
    rand = novoRand

    optBoard match {
      case Some(board) =>
        game = Game(board, novaLista, White, rand, game.rows, game.cols, game.difficulty)
        selected = None
        multiJumpPiece = None
      case None =>
        game = Game(game.board, game.lstOpenCoords, White, rand, game.rows, game.cols, game.difficulty)
        selected = None
        multiJumpPiece = None
    }
  }

  def select(row: Int, col: Int, currentDiff: Int): Boolean = {
    if (game.player != Black || isGameOver) {
      return false
    }

    val clickedCoord = (row, col)
    val diffEnum = intToDifficulty(currentDiff)

    multiJumpPiece match {
      case Some(fromPiece) =>
        if (Game.isValidMove(game.player, fromPiece, clickedCoord, game.board, game.lstOpenCoords)) {
          val (optBoard, newOpen) = game.play(fromPiece, clickedCoord)

          optBoard match {
            case Some(board) =>

              val canContinue = Game.validMoves(newOpen, game.player, board).exists(_._1 == clickedCoord)
              if (canContinue) {
                game = Game(board, newOpen, game.player, rand, game.rows, game.cols, diffEnum)
                multiJumpPiece = Some(clickedCoord)
              } else {
                game = Game(board, newOpen, White, rand, game.rows, game.cols, diffEnum)
                multiJumpPiece = None
              }
              selected = None
              true
            case None => false
          }
        } else {
          false
        }

      case None =>
        selected match {
          case None =>
            game.board.get(clickedCoord) match {
              case Some(stone) if stone == game.player =>
                selected = Some(clickedCoord)
                true
              case _ => false
            }

          case Some(from) =>
            if (Game.isValidMove(game.player, from, clickedCoord, game.board, game.lstOpenCoords)) {
              val (optBoard, newOpen) = game.play(from, clickedCoord)

              optBoard match {
                case Some(board) =>
                  history = game :: history
                  val canContinue = Game.validMoves(newOpen, game.player, board).exists(_._1 == clickedCoord)
                  if (canContinue) {
                    game = Game(board, newOpen, game.player, rand, game.rows, game.cols, diffEnum)
                    multiJumpPiece = Some(clickedCoord)
                  } else {
                    game = Game(board, newOpen, White, rand, game.rows, game.cols, diffEnum)
                    multiJumpPiece = None
                  }
                  selected = None
                  true
                case None =>
                  selected = None
                  false
              }
            } else {

              //Se a peça selecionada for movimento invalido mas for do mesmo jogador ainda, seleciona
              game.board.get(clickedCoord) match {
                case Some(stone) if stone == game.player =>
                  selected = Some(clickedCoord)
                  true
                case _ =>
                  selected = None
                  false
              }
            }
        }
    }
  }

  def stopCapturing(): Unit = {
    if (game.player == Black && multiJumpPiece.isDefined) {
      game = Game(game.board, game.lstOpenCoords, White, rand, game.rows, game.cols, game.difficulty)
      multiJumpPiece = None
      selected = None
    }
  }

  def isMultiJumping: Boolean = multiJumpPiece.isDefined

  def undo(): Boolean = {
    history match {
      case head :: tail =>
        game = head
        history = tail
        selected = None
        multiJumpPiece = None
        true
      case Nil =>
        false
    }
  }

  def reset(): Unit = {
    val (board, open) = Game.initBoard(rows, cols)
    game = Game(board, open, Black, rand, rows, cols, intToDifficulty(difficultyInt))
    selected = None
    multiJumpPiece = None
    history = Nil
  }

  def jogarComputador(): Unit = {
    if (game.player != White || isGameOver) {
      return
    }

    game.difficulty match {
      case Easy =>
        // Modo Fácil: Executa jogadas aleatórias passo a passo usando playRandomly
        var continuarLoop = true
        var pecaAtualIA: Option[Coord2D] = None

        while (continuarLoop && Game.validMoves(game.lstOpenCoords, White, game.board).nonEmpty) {
          pecaAtualIA match {
            case None =>
              val (optBoard, novoRand, novaLista, optTo) = game.playRandomly()
              rand = novoRand
              optBoard match {
                case Some(board) if optTo.isDefined =>
                  val destino = optTo.get
                  val canContinue = Game.validMoves(novaLista, White, board).exists(_._1 == destino)
                  if (canContinue) {
                    game = Game(board, novaLista, White, rand, game.rows, game.cols, game.difficulty)
                    pecaAtualIA = Some(destino)
                  } else {
                    game = Game(board, novaLista, Black, rand, game.rows, game.cols, game.difficulty)
                    continuarLoop = false
                  }
                case _ => continuarLoop = false
              }

            case Some(fromPiece) =>
              val destinations = List(
                (fromPiece._1 - 2, fromPiece._2), (fromPiece._1 + 2, fromPiece._2),
                (fromPiece._1, fromPiece._2 - 2), (fromPiece._1, fromPiece._2 + 2)
              ).filter(to => Game.isValidMove(White, fromPiece, to, game.board, game.lstOpenCoords))

              if (destinations.isEmpty) {
                game = Game(game.board, game.lstOpenCoords, Black, rand, game.rows, game.cols, game.difficulty)
                continuarLoop = false
              } else {
                val (n, nextRand) = rand.nextInt
                rand = nextRand
                val proximoDestino = destinations(math.abs(n) % destinations.length)

                val (optBoard, novaLista) = game.play(fromPiece, proximoDestino)
                optBoard match {
                  case Some(board) =>
                    val canContinue = Game.validMoves(novaLista, White, board).exists(_._1 == proximoDestino)
                    if (canContinue) {
                      game = Game(board, novaLista, White, rand, game.rows, game.cols, game.difficulty)
                      pecaAtualIA = Some(proximoDestino)
                    } else {
                      game = Game(board, novaLista, Black, rand, game.rows, game.cols, game.difficulty)
                      continuarLoop = false
                    }
                  case None => continuarLoop = false
                }
              }
          }
        }

      case Medium | Hard =>
        // Modos Medio e Dificil: Consumem diretamente os caminhos calculados pelas funcoes em Game.scala
        val moves = if (game.difficulty == Medium) game.playMediumPath() else game.playHardPath()

        if (moves.nonEmpty) {
          var currentBoard = game.board
          var currentOpen = game.lstOpenCoords
          var success = true

          for ((from, to) <- moves if success) {
            val (optB, nextOpen) = Game.play(currentBoard, White, from, to, currentOpen)
            optB match {
              case Some(b) =>
                currentBoard = b
                currentOpen = nextOpen
              case None =>
                success = false
            }
          }
          game = Game(currentBoard, currentOpen, Black, rand, game.rows, game.cols, game.difficulty)
        } else {
          game = Game(game.board, game.lstOpenCoords, Black, rand, game.rows, game.cols, game.difficulty)
        }
    }

    if (game.player == White) {
      game = Game(game.board, game.lstOpenCoords, Black, rand, game.rows, game.cols, game.difficulty)
    }
  }
}