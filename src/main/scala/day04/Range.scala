package day04

import cats.parse.Parser
import cats.parse.Rfc5234.*
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

  def toVector(range: Range): Vector[Int] =
    range.start
      .to(range.end)
      .toVector

}
