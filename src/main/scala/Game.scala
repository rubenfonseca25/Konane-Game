import scala.annotation.tailrec

case class Game(board: Board, lstOpenCoords: List[Coord2D], player: Stone, rows: Int, cols: Int) {

  def randomMove(rand: MyRandom): (Coord2D, MyRandom) = {
    Game.randomMove(this.lstOpenCoords, rand)
  }

  def play(coordFrom: Coord2D, coordTo: Coord2D,
  lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    Game.play(this.board, this.player, coordFrom, coordTo, this.lstOpenCoords)
  }

  def playRandomly(r: MyRandom): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    Game.playRandomly(this.board, r, this.player, this.lstOpenCoords, Game.randomMove)
  }
}

object Game {

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
    val indexes = lstValidCoords.indices.map(i => (0,i)).toList
    val (coordTo, newRand) = f(indexes, r)
    val coord = lstValidCoords(coordTo._2)
    val boardNList = play(board, player, coord._1, coord._2, lstOpenCoords)
    (boardNList._1, newRand, boardNList._2, Some(coord._2))
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

  private def validMoves(lstOpenCoords: List[Coord2D], player: Stone, board: Board): List[(Coord2D, Coord2D)] = {
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

  private def isValidMove(player: Stone, coordFrom: Coord2D, coordTo: Coord2D, board: Board, lstOpenCoords: List[Coord2D]): Boolean = {
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