package day25

import day22.Day22.inputLocation
import spire.math.Natural

import scala.io.Source
import util.chaining.*

object Day25 {

  val inputLocation: String = "real/day25.txt"

  val input: List[Snafu] =
    Source
      .fromResource(inputLocation)
      .getLines()
      .iterator
      .flatMap(Snafu.fromString)
      .toList

  @main
  def solution1: Unit =
    val sum = input
      .map(Snafu.toNumber)
      .sum
      .pipe(Natural.apply)
      .pipe(Snafu.fromNumber)
    pprint.log(Snafu.toString(sum))

}
