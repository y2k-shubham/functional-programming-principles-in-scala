package week_4

abstract class Bool {
  def ifThenElse[T](te: => T, ee: => T): T

  def && (x: => Bool): Bool = ifThenElse[Bool](x, False)
  def || (x: => Bool): Bool = ifThenElse[Bool](True, x)
  def unary_! : Bool = ifThenElse[Bool](False, True)

  def == (x: => Bool): Bool = ifThenElse[Bool](x, x.unary_!)
  def != (x: => Bool): Bool = ifThenElse[Bool](x.unary_!, x)

  def < (x: => Bool): Bool = ifThenElse[Bool](False, x)
}

object True extends Bool {
  override def ifThenElse[T](te: => T, ee: => T): T = te

  override def toString: String = "True"
}

object False extends Bool {
  override def ifThenElse[T](te: => T, ee: => T): T = ee

  override def toString: String = "False"
}

/*

following tests work on Scala 2.11.11

// test &&
False && False
False && True
True && False
True && True

// test ||
False || False
False || True
True || False
True || True

// test !
!False
!True

// test ==
False == False
False == True
True == False
True == True

// test !=
False != False
False != True
True != False
True != True

// test <
False < False
False < True
True < False
True < True
*/
