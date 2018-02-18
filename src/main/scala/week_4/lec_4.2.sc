
trait List[T] {
  def isEmpty: Boolean

  def head: T
  def tail: List[T]
}

class Const[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = s"$head $tail"
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head = throw new NoSuchElementException("Nil.head")
  override def tail = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "-"
}

object List {
  def apply[T]: List[T] = new Nil[T]

  def apply[T](eles: T*): List[T] = {
    eles.foldLeft[List[T]](new Nil[T]) { (list: List[T], ele: T) =>
      new Const[T](ele, list)
    }
  }
}

List[Int]
List[Int](2, 3, 4)
