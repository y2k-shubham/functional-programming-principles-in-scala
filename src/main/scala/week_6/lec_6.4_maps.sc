object test {
  val romanNums: Map[String, Int] = Map(
    "I" -> 1,
    "II" -> 2,
    "III" -> 3,
    "IV" -> 4
  )

  val capitals: Map[String, String] = Map(
    "Belgium" -> "Brussels",
    "Czech Republic" -> "Prague",
    "Bulgaria" -> "Minsk",
    "Switzerland" -> "Bern",
    "Ukraine" -> "Kiev"
  )
  capitals.get("Canada")
  capitals.get("Bulgaria")

  capitals.keys.toList.sorted
  capitals.keys.toList.sortBy(_.length)
  capitals.values.toList.sortWith(_.length <= _.length)

  capitals.keys.toList.groupBy(_.head)
}

class Poly(termsParam: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms: Map[Int, Double] = termsParam.withDefaultValue(0.0d)

  def add1(other: Poly): Poly = {
    val newMap: Map[Int, Double] = other.terms.toList.
      foldLeft[Map[Int, Double]](terms) { (map: Map[Int, Double], p: (Int, Double)) =>
      // here map.getOrElse(p._1, 0.0d) can be replaced with map(p._1)
      map + (p._1 -> (p._2 + map.getOrElse(p._1, 0.0d)))
    }
    new Poly(newMap)
  }
  def adjust(pow: Int, coeff: Double): (Int, Double) = pow -> (coeff + terms(pow))

  def add2(other: Poly): Poly = {
    new Poly(terms ++ other.terms.map(p => adjust(p._1, p._2)))
  }

  override def toString: String = {
    terms.toList.sorted.
      map { p: (Int, Double) =>
        s"${p._2}x^${p._1}"
      }.mkString(" + ")
  }
}

val p1: Poly = new Poly(Map[Int, Double](
  1 -> 2.0,
  3 -> 4.0,
  5 -> 6.2
))
val p2: Poly = new Poly(
  0 -> 3.0,
  3 -> 7.0
)

p1.add1(p2)
p1.terms(15)

p2.add2(p1)
