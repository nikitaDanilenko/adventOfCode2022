package day13

import cats.Order
import cats.parse.Parser
import utils.ParserUtil
import io.circe.{ Decoder, Encoder }
import cats.implicits.*

import scala.annotation.tailrec

sealed trait Element

object Element {

  case class Single(int: Int) extends Element

  object Single {
    implicit val decoder: Decoder[Single] = Decoder.decodeInt.map(Single.apply)
    implicit val encoder: Encoder[Single] = Encoder.encodeInt.contramap(_.int)
  }

  case class Multiple(elements: List[Element]) extends Element

  object Multiple {
    implicit val decoder: Decoder[Multiple] = Decoder.decodeList[Element].map(Multiple.apply)
  }

  implicit lazy val decoder: Decoder[Element] =
    Single.decoder.map(x => x: Element).or(Multiple.decoder.map(x => x: Element))

  implicit val order: Order[Element] = {
    @tailrec
    def compare(element1: Element, element2: Element): Boolean =
      (element1, element2) match
        case (Single(int1), Single(int2))               => int1 < int2
        case (Multiple(elements1), Multiple(elements2)) => elements1 < elements2
        case (s @ Single(_), m) =>
          compare(Multiple(List(s)), m)
        case (m, s @ Single(_)) =>
          compare(m, Multiple(List(s)))

    Order.fromLessThan(compare)
  }

}
