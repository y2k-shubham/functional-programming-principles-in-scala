
trait List[T] {
  def isEmpty: Boolean
  def length: Int
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

  def mergeSort(comp: T => Int): List[T] = mergeSortUtil(comp)(this)

  private def mergeSortUtil(comp: T => Int)(xs: List[T]): List[T] = {
    val halfLen: Int = xs.length / 2

    if (halfLen == 0) xs
    else {
      val (fPart, sPart): (List[T], List[T]) = splitN(halfLen)
      mergeUtil(comp)(fPart.mergeSort(comp), sPart.mergeSort(comp))
    }
  }

  def splitN(n: Int): (List[T], List[T]) = {
    if (isEmpty) throw new Exception("Nil.splitN")
    else splitN(n, this)
  }

  private def splitN(n: Int, xs: List[T]): (List[T], List[T]) = {
    if (n == 0) (new Nil[T], xs)
    else if (xs.isEmpty) throw new Exception("Split not possible")
    else {
      if (n == 1) (new NonEmpty[T](xs.head, new Nil[T]), xs.tail)
      else {
        val (fPart, sPart): (List[T], List[T]) = splitN(n - 1, xs.tail)
        (new NonEmpty[T](xs.head, fPart), sPart)
      }
    }
  }

  def merge(comp: T => Int)(ys: List[T]): List[T] = mergeUtil(comp)(this, ys)

  private def mergeUtil(comp: T => Int)(xs: List[T], ys: List[T]): List[T] = {
    if (xs.isEmpty) ys
    else if (ys.isEmpty) xs
    else {
      val xsNonEmpty: NonEmpty[T] = xs.asInstanceOf[NonEmpty[T]]
      val ysNonEmpty: NonEmpty[T] = ys.asInstanceOf[NonEmpty[T]]

      if (comp(xsNonEmpty.head) < comp(ysNonEmpty.head)) {
        new NonEmpty[T](xsNonEmpty.head, mergeUtil(comp)(xsNonEmpty.tail, ysNonEmpty))
      } else {
        new NonEmpty[T](ysNonEmpty.head, mergeUtil(comp)(xsNonEmpty, ysNonEmpty.tail))
      }
    }
  }

  def map(f: T => T): List[T] = map(this)(f)

  private def map(list: List[T])(f: T => T): List[T] = {
    if (list.isEmpty) list
    else new NonEmpty[T](f(list.head), map(list.tail)(f))
  }

  def transform[U](f: T => U): List[U] = transform[U](this)(f)

  private def transform[U](list: List[T])(f: T => U): List[U] = {
    if (list.isEmpty) new Nil[U]
    else new NonEmpty[U](f(list.head), transform[U](list.tail)(f))
  }

  def filter(f: T => Boolean): List[T] = filter(this)(f)

  private def filter(list: List[T])(f: T => Boolean): List[T] = {
    if (list.isEmpty) list
    else if (f(list.head)) new NonEmpty[T](list.head, filter(list.tail)(f))
    else filter(list.tail)(f)
  }

  def filterNot(f: T => Boolean): List[T] = filterNot(this)(f)

  private def filterNot(list: List[T])(f: T => Boolean): List[T] = {
    if (list.isEmpty) list
    else if (f(list.head)) filterNot(list.tail)(f)
    else new NonEmpty[T](list.head, filterNot(list.tail)(f))
  }

  def partition(f: T => Boolean): (List[T], List[T]) = (filter(this)(f), filterNot(this)(f))

  def takeWhile(f: T => Boolean): List[T] = takeWhile(this)(f)

  private def takeWhile(list: List[T])(f: T => Boolean): List[T] = {
    if (list.isEmpty) list
    else if (f(list.head)) new NonEmpty[T](list.head, takeWhile(list.tail)(f))
    else new Nil[T]
  }

  def dropWhile(f: T => Boolean): List[T] = dropWhile(this)(f)

  private def dropWhile(list: List[T])(f: T => Boolean): List[T] = {
    if (list.isEmpty) list
    else if (f(list.head)) dropWhile(list.tail)(f)
    else new NonEmpty[T](list.head, list.tail)
  }

  def span(f: T => Boolean): (List[T], List[T]) = (takeWhile(f), dropWhile(f))

  def pack(f: (T, T) => Boolean): List[List[T]] = packUtil(this)(f)

  private def packUtil(list: List[T])(f: (T, T) => Boolean): List[List[T]] = {
    if (list.isEmpty) new Nil[List[T]]
    else {
      val nonEmptyList: NonEmpty[T] = list.asInstanceOf[NonEmpty[T]]
      def g(t1: T)(t2: T): Boolean = f(t1, t2)

      val (fPart, sPart): (List[T], List[T]) = nonEmptyList.span(g(nonEmptyList.head))
      new NonEmpty[List[T]](fPart, packUtil(sPart)(f))
    }
  }

  def encode(f: (T, T) => Boolean): List[(T, Int)] = {
    if (isEmpty) new Nil[(T, Int)]
    else {
      val packedList: List[List[T]] = pack(f)
      packedList.transform[(T, Int)] { (list: List[T]) =>
        (list.head, list.length)
      }
    }
  }

  def reduce[U](v: U)(f: T => U)(g: (U, U) => U): U = {
    if (isEmpty) v
    else g(f(head), tail.reduce[U](v)(f)(g))
  }
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def length: Int = 0

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

  override def length: Int = 1 + tail.length

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

class NonEmptyInt(override val head: Int, override val tail: List[Int]) extends NonEmpty[Int](head, tail) {
  def squareList: List[Int] = map(x => x * x)
}

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

//l_5_7_3.last
//l_5.last
//
//l_1_5.init
//l_5_7_3.init
//
//l_1_5.concat(l_3).concat(l_5_7_3)
//nil.concat(l_5_7_3)
//l_5_7_3.concat(nil)
//
//l_5_7_3.reverse
//l_1_5.reverse
//nil.reverse
//
//l_5.remove(1)
//l_1_5.remove(1)
//l_1_5.remove(2)
//l_5_7_3.remove(2)

//l_3.length
//l_5_7_3.length
//nil.length
//
//l_3.splitN(0)
//l_3.splitN(1)
//l_7_3.splitN(1)
//l_5_7_3.splitN(2)
//
//l_1_5.merge(x => x)(l_5_7_3.sorted(x => x))
//l_7_3.sorted(x => x).merge(x => x)(l_1_5).merge(x => x)(l_3)
//
//l_5_7_3.concat(l_1_5).mergeSort(x => x)

//l_5_7_3.map(x => 2 * x)
//l_5_7_3.asInstanceOf[NonEmptyInt].squareList
//
//l_5_7_3.filter(_ > 4)
//l_5_7_3.filter(_ > 10)
//
//l_5_7_3.filterNot(_ > 4)
//l_5_7_3.filterNot(_ > 10)
//
//l_5_7_3.partition(_ > 4)
//l_5_7_3.partition(_ < 0)
//
//l_5_7_3.takeWhile(_ > 4)
//l_5_7_3.takeWhile(_ < 4)
//
//l_5_7_3.dropWhile(_ > 4)
//l_5_7_3.dropWhile(_ < 4)
//
//l_5_7_3.span(_ >= 5)

val l_5_5_7_3 = new NonEmptyInt(5, l_5_7_3)
val l_3_5_5_7_3 = new NonEmptyInt(3, l_5_5_7_3)
val l_3_3_5_5_7_3 = new NonEmptyInt(3, l_3_5_5_7_3)
val l_7_3_3_5_5_7_3 = new NonEmptyInt(7, l_3_3_5_5_7_3)

//l_7_3_3_5_5_7_3.dropWhile(_ != 5)
//l_7_3_3_5_5_7_3.pack((x, y) => x == y)
//l_7_3_3_5_5_7_3.encode((x, y) => x == y)

l_7_3_3_5_5_7_3.reduce[Int](0)(x => x)(_ + _)
l_7_3_3_5_5_7_3.reduce[Int](1)(x => x)(_ * _)
