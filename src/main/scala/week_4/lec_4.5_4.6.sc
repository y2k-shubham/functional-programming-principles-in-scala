trait Expr {
  def isNum: Boolean  // classification
  def numValue: Int   // accessor

  def isSum: Boolean  // classification
  def leftOp: Expr    // accessor
  def rightOp: Expr   // accessor

  def eval3: Int
}

object Expr {
  def eval(e: Expr): Int = {
    if (e.isNum) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error(s"Unknown Expression: $e")
  }

  def eval2(e: Expr): Int = {
    if (e.isInstanceOf[Number]) e.asInstanceOf[Number].numValue
    else if (e.isInstanceOf[Sum]) {
      val eSum: Sum = e.asInstanceOf[Sum]
      eval(eSum.leftOp) + eval(eSum.rightOp)
    } else throw new Error(s"Unknown Expression: $e")
  }

  /*
  Can also be written as
  def eval4(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case _ => throw new Error(s"Unknown Expression: $e")
  }

  If no expression matches, we get MatchError
  To overcome it, case _ as added
  */
  def eval4(e: Expr): Int = e match {
    case number: Number => number.numValue
    case eSum: Sum =>
      eval(eSum.leftOp) + eval(eSum.rightOp)
    case _ => throw new Error(s"Unknown Expression: $e")
  }
}

class Number(n: Int) extends Expr {
  override def isNum: Boolean = true
  override def numValue: Int = n

  override def isSum: Boolean = false
  override def leftOp: Expr = throw new Error("Number.leftOp")
  override def rightOp: Expr = throw new Error("Number.rightOp")

  override def eval3: Int = n

  override def toString: String = n.toString
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNum: Boolean = false
  override def numValue: Int = throw new Error("Sum.numValue")

  override def isSum: Boolean = true
  override def leftOp: Expr = e1
  override def rightOp: Expr = e2

  override def eval3: Int = e1.eval3 + e2.eval3

  override def toString: String = s"${Expr.eval(e1)} + ${Expr.eval(e2)}"
}



val n1: Expr = new Number(1)
val n2: Expr = new Number(2)
val s1_2: Expr = new Sum(n1, n2)
Expr.eval(s1_2)
