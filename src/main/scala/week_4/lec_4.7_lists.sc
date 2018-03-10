
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
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "Nil"
}

object NilInt extends Nil[Int]
object NilString extends Nil[String]

class NonEmpty[T](override val head: T, override val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = s"$head -> $tail"
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
