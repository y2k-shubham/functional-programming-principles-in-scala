abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
}

/**
  * changed from object to class to ensure that
  * only a single instance of Empty is
  */
object Empty extends IntSet {
  override def contains(x: Int): Boolean = false

  override def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "."
}

class NonEmpty(ele: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if (x < ele) left contains x
    else if (x > ele) right contains x
    else true
  }

  override def include(x: Int): IntSet = {
    if (x < ele) new NonEmpty(ele, left include x, right)
    else if (x > ele) new NonEmpty(ele, left, right include x)
    else this
  }

  override def toString: String = "{" + left + ele + right + "}"
}

println("Welcome to Scala worksheet")
val t1 = new NonEmpty(1, Empty, Empty)
val t2 = Empty
val t3 = t1 include 2
val t4 = t3 include 5
