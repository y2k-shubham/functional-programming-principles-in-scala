package week_1

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r)) {
      1
    } else if ((c == 1) || (c == r - 1)) {
      r
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def isBalanced(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        count == 0
      } else if (!Seq('(', ')').contains(chars.head)) {
        isBalanced(chars.tail, count)
      } else if (chars.head == '(') {
        isBalanced(chars.tail, count + 1)
      } else {
        if (count > 0) {
          isBalanced(chars.tail, count - 1)
        } else {
          false
        }
      }
    }

    isBalanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (coins.isEmpty) {
      0
    } else {
      var ctrWays = 0
      var ctrDen = 0

      val coinsSorted = coins.sorted
      while ((ctrDen * coinsSorted.head) <= money) {
        ctrWays += countChange(money - (ctrDen * coinsSorted.head), coinsSorted.tail)
        ctrDen += 1
      }

      ctrWays
    }
  }
}
