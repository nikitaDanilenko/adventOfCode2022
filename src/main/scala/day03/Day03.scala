package day03

import scala.io.Source
import scala.util.chaining._

object Day03 {

  val input: Iterator[String] =
    Source
      .fromResource("day03.txt")
      .getLines()

  private val charValues: Map[Char, Int] =
    ('a'.to('z') ++ 'A'.to('Z')).zipWithIndex.map {
      case (c, i) => c -> (1 + i)
    }.toMap

  @main
  def solution1(): Unit =
    input
      .flatMap { string =>
        val (left, right) = string.splitAt(string.length / 2)
        val common = left.toSet.intersect(right.toSet)
        common.map(charValues.apply)
      }
      .sum
      .pipe(pprint.log(_))

}
