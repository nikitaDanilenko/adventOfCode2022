package day13

import scala.io.Source
import utils.CollectionUtil.syntax._
import cats.syntax.order._
import scala.util.chaining._

object Day13 {

  val input: Seq[(Element, Element)] =
    Source
      .fromResource("real/day13.txt")
      .getLines()
      .toList
      .splitOn("")
      .map { ls =>
        ls
          .flatMap(io.circe.parser.decode[Element](_).toOption)
          .toList
      }
      .collect { case e1 :: e2 :: _ => (e1, e2) }

  @main
  def solution1(): Unit =
    input.zipWithIndex
      .collect { case ((e1, e2), i) if e1 <= e2 => 1 + i }
      .sum
      .pipe(pprint.log(_))

}
