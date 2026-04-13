import scala.collection.parallel.immutable.ParMap

type Coord2D = (Int, Int) //(row, column)
type Board = ParMap[Coord2D, Stone]
//Enum in Scala 3
enum Stone:
  case Black, White