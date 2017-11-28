import math.abs

object exercise {
  val epsilon = 0.0001d

  def isCloseEnough(xPrev: Double, xNext: Double): Boolean = {
    (abs(xNext - xPrev) / xPrev) < epsilon
  }

  def fixedPoint(f: Double => Double)(initialGuess: Double): Double = {
    def iterate(xPrev: Double): Double = {
      val xNext = f(xPrev)
      if (isCloseEnough(xPrev, xNext)) xNext
      else iterate(xNext)
    }

    iterate(initialGuess)
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrt(x: Double): Double =
    fixedPoint(averageDamp(y => x / y))(1.0)
}

exercise.fixedPoint(x => 1 + (x / 2))(1)
exercise.sqrt(2.0)