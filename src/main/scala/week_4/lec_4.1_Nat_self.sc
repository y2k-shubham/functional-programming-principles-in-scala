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

  override def successor: Nat = One

  override def + (that: Nat): Nat = that

  override def - (that: Nat): Nat = {
    if (that.isZero) this
    else throw new NoSuchElementException("Subtraction from Zero")
  }

  override def toString: String = "0"
}

object One extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = Zero

  override def + (that: Nat): Nat = that.successor

  override def - (that: Nat): Nat = {
    if (that == Zero) One
    else if (that == One) Zero
    else throw new NoSuchElementException("Negative Number")
  }

  override def toString: String = "1"
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def + (that: Nat): Nat = new Succ(n + that)

  override def - (that: Nat): Nat = {
    if (that.isZero) this
    else if (that == One) n
    else new Succ(n - that)
  }

  override def toString: String = s"1$n"
}

Zero.isZero
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
