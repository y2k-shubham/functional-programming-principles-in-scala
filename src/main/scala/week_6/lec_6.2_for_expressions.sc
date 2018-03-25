object test {
  def isPrime(n: Int): Boolean = (2 to Math.ceil(Math.sqrt(n)).toInt).forall(n % _ != 0)
  def findPairs1(n: Int): Seq[(Int, Int)] = {
    (1 until n).flatMap { i: Int =>
      (1 until i).map((i, _))
    }.filter(p => isPrime(p._1 + p._2))
  }
  def findPairs2(n: Int): Seq[(Int, Int)] = {
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)
  }
  findPairs1(7)
  findPairs2(7)

  def scalarProduct(l1: List[Double], l2: List[Double]): Double = {
    (for ((e1, e2) <- l1 zip l2) yield e1 * e2).sum
  }
  scalarProduct(List(4.5, -8.9, 2.4), List(5.7, 0.2, -3.1, 9.1))
}
