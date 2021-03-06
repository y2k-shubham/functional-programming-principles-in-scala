abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(otherSet: IntSet): IntSet
}

/**
  * changed from class to object to ensure that
  * only a single instance of Empty is
  */
object Empty extends IntSet {
  override def contains(x: Int): Boolean = false

  override def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(otherSet: IntSet): IntSet = otherSet

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

  override def union(otherSet: IntSet): IntSet =
    ((left union right) union otherSet) include ele

  override def toString: String = "{" + left + ele + right + "}"
}

println("Welcome to Scala worksheet")
val t1: IntSet = new NonEmpty(1, Empty, Empty)
val t2: IntSet = Empty
val t3: IntSet = t1 include 2
val t4: IntSet = t3 include 5
