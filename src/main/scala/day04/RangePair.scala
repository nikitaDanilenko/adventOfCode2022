package day04

import cats.parse.Parser
import scala.util.chaining._

case class RangePair(
    first: Range,
    second: Range
)

object RangePair {

  val parser: Parser[RangePair] =
    for {
      first <- Range.parser
      _ <- Parser.char(',')
      second <- Range.parser
    } yield RangePair(first, second)

  def containsAny(rangePair: RangePair): Boolean = {
    val first = rangePair.first.pipe(Range.toVector)
    val second = rangePair.second.pipe(Range.toVector)
    val intersection = first.intersect(second)
    intersection == first || intersection == second
  }

  def overlaps(rangePair: RangePair): Boolean =
    rangePair.first
      .pipe(Range.toVector)
      .intersect(rangePair.second.pipe(Range.toVector))
      .nonEmpty

}
