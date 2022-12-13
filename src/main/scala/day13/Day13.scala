package day13

import scala.io.Source
import utils.CollectionUtil.syntax._
import cats.syntax.order._
import scala.util.chaining._
import spire.compat._

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

  @main
  def solution2(): Unit =
    val divider1 = Element.Multiple(List(Element.Multiple(List(Element.Single(2)))))
    val divider2 = Element.Multiple(List(Element.Multiple(List(Element.Single(6)))))
    val allElements = List(divider1, divider2) ++ Source
      .fromResource("real/day13.txt")
      .getLines()
      .filter(_.nonEmpty)
      .flatMap(io.circe.parser.decode[Element](_).toOption)
    val sorted = allElements.sorted
    val result = (1 + sorted.indexOf(divider1)) * (1 + sorted.indexOf(divider2))
    pprint.log(result)

}
