abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nothing = throw new NoSuchElementException("Zero.predecessor")

  override def + (that: Nat): Nat = that

  override def - (that: Nat): Nat = {
    if (that.isZero) this
    else throw new NoSuchElementException("Subtraction from Zero")
  }

  override def toString: String = "0"
}


class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def + (that: Nat): Nat = new Succ(n + that)

  override def - (that: Nat): Nat = {
    if (that.isZero) this
    else new Succ(n - that.predecessor)
  }

  override def toString: String = if (n.isZero) "1" else s"1$n"
}

Zero.isZero
val One: Nat = Zero.successor
One.isZero

//Zero.predecessor
Zero.successor

One.predecessor
One.successor

Zero + Zero
Zero + One
One + Zero
One + One

Zero - Zero
//Zero - One
One - Zero
One - One

One + One + One
