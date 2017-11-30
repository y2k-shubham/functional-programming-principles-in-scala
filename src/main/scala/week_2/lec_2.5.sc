class Rational(protected val num: Int, protected val den: Int) {
  def reciprocal: Rational = Rational.reciprocalling(this)
  def invert: Rational = Rational.inversion(this)
  def reduce: Rational = Rational.reduction(this)

  def add(f: Rational): Rational = Rational.sum(this, f)
  def subtract(f: Rational): Rational = Rational.subtraction(this, f)

  def multiply(f: Rational): Rational = Rational.multiplication(this, f)
  def divide(f: Rational): Rational = Rational.division(this, f)

  def equals(f: Rational): Boolean = Rational.areEqual(this, f)

  def show(): Unit = println(this.toString)
  override def toString: String = s"$num/$den"
}

object Rational {

  def hcf(a: Int, b: Int): Int = {
    if (a < b) hcf(b, a)

    if (a % b == 0) b
    else hcf(b, a % b)
  }
  def lcm(a: Int, b: Int): Int = {
    (a * b) / hcf(a, b)
  }

  def reciprocalling(f: Rational): Rational = {
    new Rational(f.den, f.num)
  }
  def inversion(f: Rational): Rational = new Rational(-f.num, f.den)
  def reduction(f: Rational): Rational = {
    val _hcf = hcf(f.num, f.den)
    new Rational(f.num / _hcf, f.den / _hcf)
  }

  private def addOrSubtract(op: Char)(f1: Rational, f2: Rational): Rational = {
    if (op == '-') {
      addOrSubtract('+')(f1, inversion(f2))
    } else {
      val _lcm = lcm(f1.den, f2.den)

      val m1 = _lcm / f1.den
      val m2 = _lcm / f2.den

      val n1 = f1.num * m1
      val n2 = f2.num * m2

      reduction(new Rational(n1 + n2, _lcm))
    }
  }
  def sum: (Rational, Rational) => Rational = addOrSubtract('+')
  def subtraction: (Rational, Rational) => Rational = addOrSubtract('-')

  def multiplication(f1: Rational, f2: Rational): Rational = {
    reduction(new Rational(f1.num * f2.num, f2.den * f2.den))
  }
  def division(f1: Rational, f2: Rational): Rational = {
    multiplication(f1, reciprocalling(f2))
  }

  def areEqual(f1: Rational, f2: Rational): Boolean = {
    val f1Red: Rational = reduction(f1)
    val f2Red: Rational = reduction(f2)

    (f1Red.num == f2Red.num) && (f1Red.den == f2Red.den)
  }
}

val f1 = new Rational(2, 3)
val f2 = new Rational(5, 17)

f1.
  add(f2).
  add(
    f2.
      multiply(f1).
      divide(
        f1.add(f1)
      )
  ).subtract(f1).
  subtract(f2)

Rational.hcf(16, 6)
Rational.hcf(16, 64)

Rational.lcm(5, 16)
Rational.lcm(52, 16)