package day01

import scala.io.Source

object Day01 {

  lazy val numberGroups = Source
    .fromResource("day01.txt")
    .getLines()
    .toList
    .
    .map(_.map(_.toInt))

  @main
  def solution1: Unit =
    val max = numberGroups
      .map(_.sum)
      .max
    pprint.log(max)

}