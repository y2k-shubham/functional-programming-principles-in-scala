object session {

  def sqrt(x: Double): Double = {
    def abs(x: Double) = if (x > 0) x else -x

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) {
        guess
      } else {
        sqrtIter(improve(guess))
      }
    }

    def isGoodEnough(guess: Double): Boolean =
      (abs((guess * guess) - x) / x) < 0.001

    def improve(guess: Double): Double =
      (guess + (x / guess)) / 2

    sqrtIter(1.0)
  }

}

session.sqrt(2)
session.sqrt(4)

// imprecise for very small numbers
session.sqrt(1e-6)

// non-termination for very large numbers
session.sqrt(1e60)