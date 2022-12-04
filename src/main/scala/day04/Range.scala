package day04

import cats.parse.Parser
import cats.parse.Rfc5234.*
import spire.math.Interval
import utils.ParserUtil

case class Range(
    start: Int,
    end: Int
)

object Range {

  val parser: Parser[Range] = for {
    start <- ParserUtil.int
    _ <- Parser.char('-')
    end <- ParserUtil.int
  } yield Range(start, end)

  def toInterval(range: Range): Interval[Int] =
    Interval.closed(
      range.start,
      range.end
    )

}
