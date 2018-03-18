
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def sorted(comp: T => Int): List[T] = iSort(comp)(this)

  private def iSort(comp: T => Int)(xs: List[T]): List[T] = {
    if (xs.isEmpty) xs
    else {
      val nonEmptyXs: NonEmpty[T] = xs.asInstanceOf[NonEmpty[T]]

      insert(comp)(nonEmptyXs.head, iSort(comp)(nonEmptyXs.tail))
    }
  }

  private def insert(comp: T => Int)(x: T, xs: List[T]): List[T] = {
    if (xs.isEmpty) new NonEmpty[T](x, new Nil[T])
    else {
      val nonEmptyXs: NonEmpty[T] = xs.asInstanceOf[NonEmpty[T]]

      if (comp(nonEmptyXs.head) < comp(x)) {
        new NonEmpty[T](nonEmptyXs.head, insert(comp)(x, nonEmptyXs.tail))
      } else {
        new NonEmpty[T](x, nonEmptyXs)
      }
    }
  }

  def last: T

  def init: List[T]

  def concat(ys: List[T]): List[T]

  def reverse: List[T]

  def remove(n: Int): List[T]
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "Nil"

  override def last: T = throw new NoSuchElementException("Nil.last")

  override def init: List[T] = throw new NoSuchElementException("Nil.init")

  override def concat(ys: List[T]): List[T] = ys

  override def reverse: List[T] = this

  override def remove(n: Int): List[T] = this
}

object NilInt extends Nil[Int]
object NilString extends Nil[String]

class NonEmpty[T](override val head: T, override val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = s"$head -> $tail"

  override def last: T = {
    if (tail.isEmpty) head
    else tail.last
  }

  override def init: List[T] = {
    if (tail.isEmpty) tail
    else new NonEmpty(head, tail.init)
  }

  override def concat(ys: List[T]): List[T] = {
    if (tail.isEmpty) new NonEmpty(head, ys)
    else new NonEmpty(head, tail.concat(ys))
  }

  override def reverse: List[T] = {
    if (tail.isEmpty) this
    else tail.reverse.concat(new NonEmpty(head, new Nil))
  }

  override def remove(n: Int): List[T] = {
    if (n == 1) tail
    else new NonEmpty(head, tail.remove(n - 1))
  }
}

class NonEmptyInt(override val head: Int, override val tail: List[Int]) extends NonEmpty[Int](head, tail)

val nil: List[Int] = NilInt
nil.isEmpty
nil.isInstanceOf[Nil[Int]]

val l_5: List[Int] = new NonEmptyInt(5, nil)
val l_3: List[Int] = new NonEmptyInt(3, NilInt)

val l_1_5: List[Int] = new NonEmptyInt(1, l_5)
val l_7_3: List[Int] = new NonEmptyInt(7, l_3)

l_7_3.sorted(x => x)

val l_5_7_3: List[Int] = new NonEmptyInt(5, l_7_3)
l_5_7_3.sorted(x => x)

l_5_7_3.last
l_5.last

l_1_5.init
l_5_7_3.init

l_1_5.concat(l_3).concat(l_5_7_3)
nil.concat(l_5_7_3)
l_5_7_3.concat(nil)

l_5_7_3.reverse
l_1_5.reverse
nil.reverse

l_5.remove(1)
l_1_5.remove(1)
l_1_5.remove(2)
l_5_7_3.remove(2)
