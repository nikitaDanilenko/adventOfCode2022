package day06

import scala.io.Source
import scala.util.chaining._

object Day06 {

  val input: String = Source
    .fromResource("day06.txt")
    .getLines()
    .toList
    .head

  @main
  def solution1(): Unit = {
    input
      .sliding(4)
      .zipWithIndex
      .collectFirst {
        case (str, i) if str.distinct == str => i + 4
      }
      .pipe(pprint.log(_))
  }

}
