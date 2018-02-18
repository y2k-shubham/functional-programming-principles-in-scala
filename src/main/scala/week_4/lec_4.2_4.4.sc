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

// earlier trait List[T] {
trait List[+T] {
  def isEmpty: Boolean

  def head: T
  def tail: List[T]

  // T being Covariant type cannot occur in parameter
  //def prepend(x: T): List[T] = new Const[T](x, this)
  // but it can be dealt via lower-bound
  def prepend[U >: T](x: U): List[U] = new Const[U](x, this)
}

class Const[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = s"$head $tail"
}


/*
earlier
class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head = throw new NoSuchElementException("Nil.head")
  override def tail = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "-"
}
*/
object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "-"
}

object List {
  def apply[T]: List[T] = {
    // earlier: new Nil[T]
    Nil
    // Nil[T] also works
  }

  def apply[T](eles: T*): List[T] = {
    /*
    earlier
    eles.foldLeft[List[T]](new Nil[T]) { (list: List[T], ele: T) =>
      new Const[T](ele, list)
    }
    */
    eles.foldLeft[List[T]](Nil) { (list: List[T], ele: T) =>
      new Const[T](ele, list)
    }
    // Nil[T] also works
  }
}

object Test {
  val x: List[String] = Nil
  def f(xs: List[NonEmpty], x: Empty.type) = xs prepend x
}

List[Int]
List[Int](2, 3, 4)
