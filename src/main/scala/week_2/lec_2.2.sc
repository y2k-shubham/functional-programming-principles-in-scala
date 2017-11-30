object curry {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def factorial(n: Int): Int = product(x => x)(1, n)

  /**
    *
    * @param op operation function decides type of
    *           operation to perform over range (addition / multiplication)
    * @param u termination value of recursion
    *          (0 for sum, 1 for multiplication)
    * @param f transformation to be applied to each value of range
    *          before performing op operation over the range
    * @param a starting value of range
    * @param b ending value of range
    * @return
    */
  def rangeOp(op: (Int, Int) => Int, u: Int)(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) u
    else op(f(a), rangeOp(op, u)(f)(a + 1, b))
  }

  def prodInTermsOfRangeOp(a: Int, b: Int): Int =
    rangeOp((x: Int, y: Int) => x * y, 1)(e => e)(a, b)
}

curry.product(x => x)(5, 8)
curry.product(x => x * x)(2, 4)

curry.factorial(5)
curry.factorial(1)

curry.rangeOp((x: Int, y: Int) => x + y, 0)(p => 2 * p)(3, 6)
curry.rangeOp((x: Int, y: Int) => x * y, 1)(p => p)(3, 6)

curry.prodInTermsOfRangeOp(2, 5)