package day01

import scala.io.Source
import utils.CollectionUtil.syntax._

object Day01 {

  lazy val numberGroups: Seq[Seq[Int]] = Source
    .fromResource("day01.txt")
    .getLines()
    .toList
    .splitOn("")
    .map(_.map(_.toInt))

  @main
  def solution1(): Unit = {
    val max = numberGroups
      .map(_.sum)
      .max
    pprint.log(max)
  }

  @main
  def solution2(): Unit = {
    val topThree = numberGroups
      .map(_.sum)
      .sortWith(_ >= _)
      .take(3)
      .sum
    pprint.log(topThree)
  }

}
