package logic

case class MyRandom(seed: Long) {
  def nextInt: (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRand = MyRandom(newSeed)
    val newInt = (newSeed >>> 16).toInt
    (newInt, newRand)
  }
}

