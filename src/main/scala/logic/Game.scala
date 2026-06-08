package logic

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import Difficulty.{Easy, Medium, Hard}

case class Game(board: Board, lstOpenCoords: List[Coord2D], player: Stone, rand: MyRandom, rows: Int, cols: Int, difficulty: Difficulty) {

  def initBoard(): (Board, List[Coord2D]) = Game.initBoard(this.rows, this.cols)

  def randomMove(): (Coord2D, MyRandom) = {
    Game.randomMove(this.lstOpenCoords, rand)
  }

  def play(coordFrom: Coord2D, coordTo: Coord2D): (Option[Board], List[Coord2D]) = {
    Game.play(this.board, this.player, coordFrom, coordTo, this.lstOpenCoords)
  }

  def playRandomly(): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    Game.playRandomly(this.board, rand, this.player, this.lstOpenCoords, Game.randomMove)
  }

  def playMediumPath(): List[(Coord2D, Coord2D)] = Game.playMedium(board, player, lstOpenCoords)

  def playHardPath(): List[(Coord2D, Coord2D)] = Game.playHard(board, player, lstOpenCoords)

  def isWinner(winner: Stone): Boolean = Game.isWinner(board, winner, lstOpenCoords)

  def isValidMove(coordFrom: Coord2D, coordTo: Coord2D): Boolean = {
    Game.isValidMove(this.player, coordFrom, coordTo, this.board, this.lstOpenCoords)
  }
}

object Game {

  def initBoard(rows: Int, cols: Int): (Board, List[Coord2D]) = {

    @tailrec
    def fill(r: Int, c: Int, acc: Board): Board = (r, c) match {
      // Caso base: Se a linha r atingiu o limite, terminamos e devolvemos o acumulador
      case (row, _) if row >= rows => acc

      // Se a coluna c atingiu o limite, passamos para a linha seguinte (coluna 0)
      case (row, col) if col >= cols => fill(row + 1, 0, acc)

      // Caso padrão: Adicionamos a peça na posição atual e avançamos na coluna
      case (row, col) =>
        val stone = if ((row + col) % 2 == 0) Stone.Black else Stone.White
        fill(row, col + 1, acc + ((row, col) -> stone))
    }

    // Criar o tabuleiro cheio (T1/T2 setup)
    val fullBoard = fill(0, 0, ParMap.empty[Coord2D, Stone])

    // Lógica de remoção inicial (Requisito da T2)
    val centerBlack = (rows / 2, cols / 2)
    val centerWhite = (rows / 2, cols / 2 - 1)

    val board = fullBoard - centerBlack - centerWhite
    val lstOpenCoords = List(centerBlack, centerWhite)

    (board, lstOpenCoords)
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val newInt = rand.nextInt._1
    val newRand = rand.nextInt._2
    val newPos = newInt % lstOpenCoords.length
    if(newPos < 0) (lstOpenCoords(-newPos), newRand)
    else (lstOpenCoords(newPos), newRand)
  }

  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D,
  lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if(!exists(coordTo, lstOpenCoords)) (None, lstOpenCoords)
    else if(!isValidMove(player, coordFrom, coordTo, board, lstOpenCoords)) (None, lstOpenCoords)
    else {
      val captured = capturedCoord(coordFrom, coordTo)
      captured match {
        case Some(capturedValue) => {
          val lst = removeCoord(coordTo, lstOpenCoords)
          val newBoard = board - capturedValue + (coordTo, player) - coordFrom
          (Some(newBoard), coordFrom :: capturedValue :: lst)
        }
        case _ => (None, lstOpenCoords)
      }
    }
  }

  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
  f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    val lstValidCoords = validMoves(lstOpenCoords, player, board)
    lstValidCoords match {
      case Nil => (None, r, lstOpenCoords, None)
      case _ => {
        val indexes = lstValidCoords.indices.map(i => (0, i)).toList
        val (coordTo, newRand) = f(indexes, r)
        val coord = lstValidCoords(coordTo._2)
        val boardNList = play(board, player, coord._1, coord._2, lstOpenCoords)
        (boardNList._1, newRand, boardNList._2, Some(coord._2))
      }
    }
  }

  def isWinner(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): Boolean = {
    val otherPlayer = player match {
      case p if p == Stone.White => Stone.Black
      case _ => Stone.White
    }
    validMoves(lstOpenCoords, otherPlayer, board).isEmpty
  }

  //Escolhe jogada de um passo em que a proxima jogada do adversario tem menos jogadas possíveis
  def playMedium(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {

    val otherPlayer = player match {
      case Stone.White => Stone.Black
      case _ => Stone.White
    }

    val moves = validMoves(lstOpenCoords, player, board)
    if (moves.isEmpty) {
      Nil
    } else {
      moves.map { case (from, to) =>
        val (optionB, lstOC) = play(board, player, from, to, lstOpenCoords)
        optionB match {
          case Some(v) =>
            (validMoves(lstOC, otherPlayer, v).length, List((from, to)))
          case None =>
            (Int.MaxValue, Nil)
        }
      }.minBy(_._1)._2
    }

  }

  def playHard(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {

    def biggestPath(board2: Board, lstOpenCoords2: List[Coord2D], fixed: Option[Coord2D]): (Int, List[(Coord2D, Coord2D)]) = {
      val moves = fixed match {
        case None => validMoves(lstOpenCoords2, player, board2)
        case Some(p) => validMoves(lstOpenCoords2, player, board2).filter(_._1 == p)
      }

      if (moves.isEmpty) {
        (0, Nil)
      } else {
        moves.map { case (from, to) =>
          val (optionB, lstOC) = play(board2, player, from, to, lstOpenCoords2)
          optionB match {
            case Some(v) =>
              val value2 = biggestPath(v, lstOC, Some(to))
              (1 + value2._1, (from, to) :: value2._2)
            case None =>
              (0, Nil)
          }
        }.maxBy(_._1)
      }
    }
    val bestP = biggestPath(board, lstOpenCoords, None)
    bestP._2
  }

  private def coordsPlayer(player: Stone, board: Board): List[Coord2D] = {
    def aux(lst: List[(Coord2D, Stone)]): List[Coord2D] = {
      lst match {
        case Nil => Nil
        case (x, y) :: t => x :: aux(t)
      }
    }
    val newBoard = board.filter((x, y) => y == player)
    val coords = newBoard.toList
    aux(coords)
  }

  def validMoves(lstOpenCoords: List[Coord2D], player: Stone, board: Board): List[(Coord2D, Coord2D)] = {
    val coords = coordsPlayer(player, board)
    def aux(lst: List[Coord2D], lst2: List[Coord2D]): List[(Coord2D, Coord2D)] = {
      (lst, lst2) match {
        //Caso em que a lista de jogadas do jogador acabam mas ainda ha casas vazias ou quando acabam os dois (Nil, Nil)
        case (Nil, _) => Nil
        case (x :: t, Nil) => aux(t, lstOpenCoords)
        case (x :: t, y :: t2) => if (isValidMove(player, x, y, board, lstOpenCoords)) (x, y) :: aux(x :: t, t2) else aux(x :: t, t2)
      }
    }
    aux(coords, lstOpenCoords)
  }

  def isValidMove(player: Stone, coordFrom: Coord2D, coordTo: Coord2D, board: Board, lstOpenCoords: List[Coord2D]): Boolean = {
    val elem = board.get(coordFrom)
    val captured = capturedCoord(coordFrom, coordTo)
    captured match {
      case Some(capturedVal) => {
        val middlePiece = board.get(capturedVal)
        if (elem.isEmpty || !elem.contains(player)) false
        else if (!(middlePiece.isDefined && !middlePiece.contains(player))) false
        else true
      }
      case _ => false
    }
    
  }

  @tailrec
  private def exists(coord: Coord2D, lst: List[Coord2D]): Boolean = {
    lst match {
      case Nil => false
      case head :: tail => if(head == coord) true else exists(coord, tail)
    }
  }

  private def capturedCoord(coord1: Coord2D, coord2: Coord2D): Option[Coord2D] = {
    (coord1, coord2) match {
      case ((x1,y), (x2,z)) if(x1 == x2) => if(y<z && (z-y == 2)) Some((x1, z-1)) else if(y>z && (y-z == 2)) Some((x1, y-1)) else None
      case ((x,y1), (z,y2)) if(y1 == y2) => if(x<z && (z-x == 2)) Some((z-1, y1)) else if(x>z && (x-z == 2)) Some((x-1, y1)) else None
      case _ => None
    }
  }

  // Explicit_recursion
  /*private def removeCoord(coord: Coord2D, lst: List[Coord2D]): List[Coord2D] = {
    lst match {
      case Nil => Nil
      case h :: t => if(h == coord) t else h :: removeCoord(coord, t)
    }
  }*/

  private def removeCoord(coord: Coord2D, lst: List[Coord2D]): List[Coord2D] = {
    lst.foldLeft(List.empty[Coord2D]) { (acc, h) =>
      if (h == coord) acc else h :: acc
    }
  }
}