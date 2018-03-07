trait Expr {
  def isNum: Boolean  // classification
  def numValue: Int   // accessor

  def isSum: Boolean     // classification
  def isProduct: Boolean // classification

  def leftOp: Expr    // accessor
  def rightOp: Expr   // accessor

  def isVar: Boolean = {
    this match {
      case variable: Var => true
      case _ => false
    }
  }
  def idName: String

  def eval3: Int
}

object Expr {
  def eval(e: Expr): Int = {
    if (e.isNum) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else if (e.isProduct) eval(e.leftOp) * eval(e.rightOp)
    else if (e.isVar) throw new Error("eval(Var)")
    else throw new Error(s"Unknown Expression: $e")
  }

  def eval2(e: Expr): Int = {
    if (e.isInstanceOf[Number]) e.asInstanceOf[Number].numValue
    else if (e.isInstanceOf[Sum]) {
      val eSum: Sum = e.asInstanceOf[Sum]
      eval2(eSum.leftOp) + eval2(eSum.rightOp)
    } else if (e.isInstanceOf[Product]) {
      val eProd: Product = e.asInstanceOf[Product]
      eval2(e.leftOp) * eval2(e.rightOp)
    } else if (e.isInstanceOf[Var]) {
      throw new Error("eval2(Var)")
    } else {
      throw new Error(s"Unknown Expression: $e")
    }
  }

  /*
  Can also be written as
  def eval4(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case _ => throw new Error(s"Unknown Expression: $e")
  }

  If no expression matches, we get MatchError
  To overcome it, case _ is added
  */
  def eval4(e: Expr): Int = e match {
    case number: Number => number.numValue
    case eSum: Sum =>
      eval4(eSum.leftOp) + eval4(eSum.rightOp)
    case eProd: Product =>
      eval4(eProd.leftOp) * eval4(eProd.rightOp)
    case eVar: Var => throw new Error("eval4(Var)")
    case _ => throw new Error(s"Unknown Expression: $e")
  }

  def show(e: Expr): String = e match {
    case number: Number => number.numValue.toString
    case eSum: Sum =>
      s"${show(eSum.leftOp)} + ${show(eSum.rightOp)}"
    case eProd: Product =>
      s"${parenthesize(eProd.leftOp)} * ${parenthesize(eProd.rightOp)}"
    case eVar: Var => eVar.idName
    case _ => throw new Error(s"Unknown Expression: $e")
  }

  private def parenthesize(expr: Expr): String = expr match {
    case eSum: Sum => s"(${show(eSum.leftOp)} + ${show(eSum.rightOp)})"
    case expr: Expr => show(expr)
  }
}

class Number(n: Int) extends Expr {
  override def isNum: Boolean = true
  override def numValue: Int = n

  override def isSum: Boolean = false
  override def isProduct: Boolean = false

  override def leftOp: Expr = throw new Error("Number.leftOp")
  override def rightOp: Expr = throw new Error("Number.rightOp")

  override def idName = throw new Error("Number.idName")

  override def eval3: Int = n

  override def toString: String = n.toString
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNum: Boolean = false
  override def numValue: Int = throw new Error("Sum.numValue")

  override def isSum: Boolean = true
  override def isProduct: Boolean = false

  override def leftOp: Expr = e1
  override def rightOp: Expr = e2

  override def idName = throw new Error("Sum.idName")

  override def eval3: Int = e1.eval3 + e2.eval3

  override def toString: String = s"$e1 + $e2"
}

class Product(e1: Expr, e2: Expr) extends Expr {
  override def isNum: Boolean = false
  override def numValue: Int = throw new Error("Product.numValue")

  override def isSum: Boolean = false
  override def isProduct: Boolean = true

  override def leftOp: Expr = e1
  override def rightOp: Expr = e2

  override def idName = throw new Error("Product.idName")

  override def eval3: Int = e1.eval3 * e2.eval3

  override def toString: String = s"$e1 * $e2"
}

class Var(id: String) extends Expr {
  override def isNum: Boolean = false
  override def numValue: Int = throw new Error("Var.numValue")

  override def isSum: Boolean = false
  override def isProduct: Boolean = false

  override def leftOp: Expr = throw new Error("Var.leftOp")
  override def rightOp: Expr = throw new Error("Var.rightOp")

  override def idName = id

  override def eval3: Int = throw new Error("Var.eval3")

  override def toString: String = id
}

val n1: Expr = new Number(1)
val n2: Expr = new Number(2)
val vx: Expr = new Var("x")

val s1_2: Expr = new Sum(n1, n2)
val s1_x: Expr = new Sum(n1, vx)
val p1_2: Expr = new Product(n1, n2)
val p2_x: Expr = new Product(n2, vx)

Expr.eval(s1_2)
Expr.eval2(s1_2)
Expr.eval4(s1_2)
s1_2.asInstanceOf[Sum].eval3

Expr.eval(p1_2)
Expr.eval2(p1_2)
Expr.eval4(p1_2)
p1_2.asInstanceOf[Product].eval3

vx.idName

Expr.show(n1)
Expr.show(n2)
Expr.show(s1_2)
Expr.show(s1_x)
Expr.show(p1_2)
Expr.show(p2_x)

val sOp1: Expr = new Sum(vx, p1_2)
val pOs1: Expr = new Product(s1_2, vx)

Expr.show(sOp1)
Expr.show(pOs1)

val sOp2: Expr = new Sum(sOp1, pOs1)
val pOs2: Expr = new Product(sOp1, pOs1)

Expr.show(sOp2)
Expr.show(pOs2)
