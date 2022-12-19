package day19

import scala.io.Source

object Day19 {

  val input: List[Blueprint] =
    Source
      .fromResource("test/day19.txt")
      .getLines()
      .flatMap(Blueprint.parser.parse(_).toOption.map(_._2))
      .toList

  @main
  def solution1(): Unit =
    pprint.log(input.size)

}
