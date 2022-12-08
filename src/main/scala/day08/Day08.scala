package day08

import scala.io.Source
import scala.util.chaining._

object Day08 {

  val input: Map[(Int, Int), Int] = Source
    .fromResource("day08.txt")
    .getLines()
    .zipWithIndex
    .flatMap { case (str, i) =>
      str.zipWithIndex.map { case (c, j) =>
        (i, j) -> s"$c".toInt
      }
    }
    .toMap

  def computeVisible(map: Map[(Int, Int), Int]): Int =
    val range = map.keys.map(_._1).max
    map.count { case ((i, j), height) =>
      val left = 0.until(j).map(index => map((i, index)))
      val right = (1 + j).to(range).map(index => map((i, index)))
      val top = 0.until(i).map(index => map((index, j)))
      val bottom = (1 + i).to(range).map(index => map((index, j)))

      List(
        left,
        right,
        top,
        bottom
      ).exists(_.forall(_ < height))
    }

  @main
  def solution1(): Unit =
    input
      .pipe(computeVisible)
      .pipe(pprint.log(_))

}
