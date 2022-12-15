package day15

sealed trait Interval

object Interval {
  case object Empty extends Interval
  sealed abstract case class NonEmpty(lower: Int, upper: Int) extends Interval

  def apply(lower: Int, upper: Int): Interval =
    Some(new NonEmpty(lower, upper) {})
      .filter(isValid)
      .getOrElse(Empty)

  private def isValid(nonEmpty: NonEmpty): Boolean =
    nonEmpty.upper >= nonEmpty.lower

  def intersect(interval1: Interval, interval2: Interval): Interval =
    (interval1, interval2) match
      case (NonEmpty(a, b), NonEmpty(c, d)) =>
        val lower = math.max(a, c)
        val upper = math.min(b, d)
        Interval(lower, upper)
      case _ => Empty

  def diff(interval1: Interval, interval2: Interval): List[Interval] =
    (interval1, interval2) match {
      case (Empty, _) => List.empty
      case (_, Empty) => List(interval1)
      case (NonEmpty(a, b), NonEmpty(c, d)) =>
        List(
          Interval(a, math.min(b, c - 1)),
          Interval(math.max(a, d + 1), b)
        ).filter(_ != Empty)
    }

  def diffAll(interval: Interval, diffs: List[Interval]): List[Interval] =
    diffs
      .foldLeft(List(interval))((is, d) => is.flatMap(diff(_, d)))
      .filter(_ != Empty)

  def length(interval: Interval): Int = interval match
    case Empty                  => 0
    case NonEmpty(lower, upper) => 1 + upper - lower

}
