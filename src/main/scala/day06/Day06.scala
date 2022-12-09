package day06

import scala.io.Source
import scala.util.chaining._

object Day06 {

  val input: String = Source
    .fromResource("real/day06.txt")
    .getLines()
    .toList
    .head

  def firstDistinctOf(
      string: String,
      size: Int
  ): Option[Int] =
    string
      .sliding(size)
      .zipWithIndex
      .collectFirst {
        case (str, i) if str.distinct == str => i + size
      }

  @main
  def solution1(): Unit = {
    firstDistinctOf(input, 4)
      .pipe(pprint.log(_))
  }

  @main
  def solution2(): Unit = {
    firstDistinctOf(input, 14)
      .pipe(pprint.log(_))
  }

}
