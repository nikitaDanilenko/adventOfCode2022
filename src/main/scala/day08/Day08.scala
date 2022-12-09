package day08

import cats.kernel.Order
import cats.syntax.order._

import scala.io.Source
import scala.util.chaining.*

object Day08 {

  type Forest = Map[(Int, Int), Int]

  val input: Forest = Source
    .fromResource("real/day08.txt")
    .getLines()
    .zipWithIndex
    .flatMap { case (str, i) =>
      str.zipWithIndex.map { case (c, j) =>
        (i, j) -> s"$c".toInt
      }
    }
    .toMap

  case class Rows(
      left: List[Int],
      right: List[Int],
      top: List[Int],
      bottom: List[Int]
  )

  def crossOf(map: Forest, range: Int, pos: (Int, Int)): Rows =
    val (i, j) = pos
    Rows(
      left = 0.until(j).map(index => map((i, index))).toList,
      right = (1 + j).to(range).map(index => map((i, index))).toList,
      top = 0.until(i).map(index => map((index, j))).toList,
      bottom = (1 + i).to(range).map(index => map((index, j))).toList
    )

  def computeVisible(map: Forest): Int =
    val range = map.keys.map(_._1).max
    map.count { case ((i, j), height) =>
      val cross = crossOf(map, range, (i, j))

      List(
        cross.left,
        cross.right,
        cross.top,
        cross.bottom
      ).exists(_.forall(_ < height))
    }

  def scenicScore(map: Forest, pos: (Int, Int), height: Int): BigInt =
    val range = map.keys.map(_._1).max
    val (i, j) = pos
    val cross = crossOf(map, range, (i, j))
    List(
      cross.left.reverse,
      cross.right,
      cross.top.reverse,
      cross.bottom
    ).map(takeWhileSmallerToAndStop(_, height).length.pipe(BigInt(_))).product

  def takeWhileSmallerToAndStop[A: Order](xs: List[A], max: A): List[A] =
    val (smaller, greaterOrEqual) = xs.span(_ < max)
    smaller ++ greaterOrEqual.headOption

  @main
  def solution1(): Unit =
    input
      .pipe(computeVisible)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    input
      .map { case ((i, j), height) => scenicScore(input, (i, j), height) }
      .max
      .pipe(pprint.log(_))

}
