import scala.collection.JavaConverters._
import scala.io.Source

object x {
  val path: String = "/Users/zomadmin/zomato/other/courses/Scala/example/src/main/scala/week_6/dict_epfl.txt"
  val words: List[String] = Source.
    fromFile(path).
    getLines().
    toList.
    map(_.toLowerCase).
    filter(_.forall(_.isLetter))

  val mnem: Map[Char, String] = Map(
    '2' -> "abc",
    '3' -> "def",
    '4' -> "ghi",
    '5' -> "jkl",
    '6' -> "mno",
    '7' -> "pqrs",
    '8' -> "tuv",
    '9' -> "wxyz"
  )

  val charCode: Map[Char, Char] = {
    mnem.toList.flatMap { p: (Char, String) =>
      p._2.map((_, p._1))
    }.sortBy(_._1).toMap
  }

  def wordCode(word: String): String = {
    word.
      toLowerCase.
      map(charCode.getOrElse(_, '?'))
  }
  wordCode("java")

  val wordsForNum: Map[String, List[String]] = {
    words.
      groupBy(wordCode).
      withDefaultValue(List.empty)
  }

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List.empty)
    else {
      // This is also correct
//      {
//        for {
//          i <- 1 to number.length
//          wordsTillI = wordsForNum(number.take(i))
//          phrasesAfterI = encode(number.drop(i))
//        } yield {
////          wordsTillI :: phrasesAfterI
//          wordsTillI.flatMap { word: String =>
//            phrasesAfterI.map(word :: _).toSet
//          }
//        }
//      }.toList.
//        flatten.
//        toSet

      (1 to number.length).flatMap { i: Int =>
        val wordsTillI = wordsForNum(number.take(i))
        val phrasesAfterI = encode(number.drop(i))
        wordsTillI.flatMap { word: String =>
          phrasesAfterI.map(word :: _)
        }
      }.toSet
    }
  }

  encode("7225247386")
}
