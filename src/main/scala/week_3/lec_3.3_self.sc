/**
  * INCORRECT: doesn't work
  * committed to git just for backup: know that this doesn't work
  */

abstract class ConstList {
  def contains(x: Any): Boolean
  def add(x: Any): ConstList
  def equals(otherList: ConstList): Boolean
  def union(otherList: ConstList): ConstList
}

object EmptyList extends ConstList {
  override def contains(x: Any): Boolean = false

  override def add(x: Any): ConstList = new NonEmptyList(x, EmptyList)

  override def equals(otherList: ConstList): Boolean = {
    if (otherList.isInstanceOf[EmptyList.type]) true
    else false
  }

  override def union(otherList: ConstList): ConstList = otherList
}

class NonEmptyList(val data: Any, val next: ConstList) extends ConstList {
  override def contains(x: Any): Boolean = {
    if (data.equals(x)) true
    else next contains x
  }

  override def add(x: Any): ConstList = new NonEmptyList(data, next add x)

  override def equals(otherList: ConstList): Boolean = {
    if (otherList.isInstanceOf[EmptyList.type]) return false

    val otherNonEmptyList: NonEmptyList = otherList.asInstanceOf[NonEmptyList]

    if (!data.equals(otherNonEmptyList.data)) false
    else next equals otherNonEmptyList.next
  }

  override def union(otherList: ConstList) = ???
}
