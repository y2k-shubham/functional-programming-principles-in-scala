import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

/**
  * Here 'val head: T' and 'val tail: List[T]' behave
  * as implementations of 'def head: T' and 'def tail: List[T]'
  */
class Const[T](override val head: T, override val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  /**
    * Here the following is implied automatically
    * head => head.toString
    * tail => tail.toString
    */
  override def toString: String = "[" + head + "]" + tail

  override def nth(n: Int): T = {
    if (n == 0) head
    else tail.nth(n - 1)
  }
}

object Const {
  def singleton[T](elem: T): Const[T] = new Const[T](elem, new Nil[T])
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "."

  override def nth(n: Int): T = throw new IndexOutOfBoundsException("Nil")
}

val listNil: Nil[Int] = new Nil[Int]
val list5: Const[Int] = Const.singleton[Int](5)

val list5Nil : Const[Int] = new Const[Int](5, listNil)
val list95: Const[Int] = new Const[Int](9, list5)
val list295: Const[Int] = new Const[Int](2, list95)

list295.nth(1)
listNil.nth(3)
