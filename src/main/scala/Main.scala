object Main extends App{
  val lst = List((1, 2), (3, 4), (5, 6), (7, 8))
  val r = MyRandom(System.currentTimeMillis())
  val n = Game.randomMove(lst, r)
  val r2 = n._2
  val n2 = Game.randomMove(lst, r2)
  val r3 = n2._2
  val n3 = Game.randomMove(lst, r3)
  println(lst)
  println(n)
  println(n2)
  println(n3)
}