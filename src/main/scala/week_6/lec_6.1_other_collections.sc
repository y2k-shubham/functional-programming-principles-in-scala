object test {
  val arr: Array[Int] = Array(3, 1, 5, 7, 5, 5)
  arr.map(_ * 2)

  val str: String = "Hello World!"
  str.filter(_.isUpper)
  str.exists(_.isSpaceChar)
  str.forall(_.isDigit)

  val seq: Seq[Int] = Seq(5, 9, 2, 3)
  val zipped: Seq[(Int, Char)] = seq.zip(str)
  val unzippedSeqs: (Seq[Int], Seq[Char]) = zipped.unzip

  str.flatMap(List(_, "."))
  arr.sum
  arr.product
  zipped.max
  str.min

  val M: Int = 4
  val N: Int = 3
  (1 to M).flatMap { i: Int =>
    (1 to N).map((i, _))
  }

  def scalarProduct1(vec1: Vector[Double], vec2: Vector[Double]): Double = {
    (vec1 zip vec2).foldLeft(0.0d) { (sop: Double, pair: (Double, Double)) =>
      sop + pair._1 * pair._2
    }
  }
  def scalarProduct2(vec1: Vector[Double], vec2: Vector[Double]): Double = {
    (vec1 zip vec2).map(p => p._1 * p._2).sum
  }
  scalarProduct1(Vector(1.9, -4.2, 0.5, 2.1), Vector(8.3, 9.1, -5.0))
  scalarProduct2(Vector(1.9, -4.2, 0.5), Vector(8.3, 9.1, -5.0))

  def isPrime1(n: Int): Boolean = {
    val lim: Int = Math.ceil(Math.sqrt(n)).toInt
    (2 to lim).forall(n % _ != 0)
  }
  def isPrime2(n: Int): Boolean = {
    (1 to n).count(n % _ == 0) == 2
  }
  isPrime1(13)
  isPrime2(12)
}
