class Rational(private val _num: Int, private val _den: Int) {
  require(_den != 0, "denominator can't be zero")

  private val _hcf = Rational.hcf(_num, _den)
  private val _sign = if (_den < 0) -1 else 1
  protected val num = _sign * _num / _hcf
  protected val den = math.abs(_den) / _hcf

  def this(_num: Int) = this(_num, 1)

  def reciprocal: Rational = Rational.reciprocalling(this)
  def invert: Rational = Rational.inversion(this)
  def reduce: Rational = Rational.reduction(this)

  def add(f: Rational): Rational = Rational.sum(this, f)
  def subtract(f: Rational): Rational = Rational.subtraction(this, f)

  def multiply(f: Rational): Rational = Rational.multiplication(this, f)
  def divide(f: Rational): Rational = Rational.division(this, f)

  def toDecimal: Double = Rational.decimal(this)
  def equals(f: Rational): Boolean = Rational.areEqual(this, f)
  def isBigger(f: Rational): Boolean = {
    Rational.decimal(this) > Rational.decimal(f)
  }

  def show(): Unit = println(this.toString)
  override def toString: String = s"$num/$den"
}

object Rational {

  def hcf(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) return 1
    if (a < 0 || b < 0) return hcf(math.abs(a), math.abs(b))
    if (a < b) return hcf(b, a)

    if (a % b == 0) b
    else hcf(b, a % b)
  }
  def lcm(a: Int, b: Int): Int = {
    math.abs((a * b) / hcf(a, b))
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

  def max(f1: Rational, f2: Rational): Rational = {
    val f11: Double = decimal(f1)
    val f22: Double = decimal(f2)

    if (f11 > f22) new Rational(f1.num, f1.den)
    else new Rational(f2.num, f2.den)
  }

  def decimal(f: Rational): Double = {
    f.num.toDouble / f.den
  }
}

val f1 = new Rational(4, 3)
val f2 = new Rational(5, 17)
val f3 = new Rational(15, 7)
val f4 = new Rational(-4, 16)

val f5 = new Rational(-17, 10)
val f6 = new Rational(-17, -10)
val f7 = new Rational(17, -10)

val f8 = new Rational(4)

f1.reciprocal
f1.invert

f1.subtract(f2)
f1.add(f2).add(f1.add(f1)).multiply(f2).divide(f1).subtract(f2)

Rational.max(f3, f4)
Rational.decimal(f3)
f4.toDecimal

f2.isBigger(f1)
