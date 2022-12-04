package day04

import scala.io.Source
import scala.util.chaining._

object Day04 {

  val input: Iterator[RangePair] = Source
    .fromResource("day04.txt")
    .getLines()
    .flatMap(
      RangePair.parser
        .parse(_)
        .map(_._2)
        .toOption
    )

  @main
  def solution1(): Unit =
    input
      .count(RangePair.containsAny)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    input
      .count(RangePair.overlaps)
      .pipe(pprint.log(_))

}
