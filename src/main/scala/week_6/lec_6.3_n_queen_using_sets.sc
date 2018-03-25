object test {
  val sFruit: Set[String] = Set("apple", "orange", "mango", "banana")
  val sInt: Set[Int] = (5 to 15 by 3).toSet

  sInt.map(_ % 2)
  sFruit.filter(_.length <= 5)
  sInt.exists(_ < 0)
  sInt zip sFruit

  sInt.contains(2)

  def queens(n: Int): Set[List[Int]] = {
    if (n < 4) Set(List.empty)
    else {
      def placeQueens(k: Int): Set[List[Int]] = {
        if (k == 0) Set(List.empty)
        else {
          for {
            partialSoln: List[Int] <- placeQueens(k - 1)
            col: Int <- 0 until n
            if isSafe(partialSoln, col)
          } yield col :: partialSoln
        }
      }

      placeQueens(n)
    }
  }

  def isSafe(soln: List[Int], col: Int): Boolean = {
    val row: Int = soln.length
    (row - 1 to 0 by -1).zip(soln).
      forall { pos: (Int, Int) =>
        pos._2 != col && (Math.abs(pos._2 - col) != Math.abs(pos._1 - row))
      }
  }

  def showSoln(soln: List[Int]): Unit = {
    val n: Int = soln.length

    val board: String = {
      soln.map { col: Int =>
        val row: String = {
          for {
            i <- 0 until n
          } yield {
            if (i == col) "Q"
            else " "
          }
        }.mkString("|")
        s"|$row|"
      }.mkString(s"\n")
    }
    println(s"\n$board")
  }

  queens(4)
  queens(4).foreach(showSoln)
}
