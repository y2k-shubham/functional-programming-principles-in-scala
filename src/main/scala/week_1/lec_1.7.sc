object exercise {

  def factorial(x: Int): Int = {
    def tailRec(n: Int, fact: Int): Int =
      if (n == 0) fact else tailRec(n - 1, n * fact)

    if (x < 0)
      throw new IllegalArgumentException
    else
      tailRec(x, 1)
  }

}

//exercise.factorial(-1)
exercise.factorial(0)
exercise.factorial(5)