package day21

import algebra.ring.Semiring
import cats.parse.Parser
import spire.algebra.EuclideanRing
import spire.syntax.ring._
import spire.syntax.euclideanRing._

enum Operation:
  case Plus, Minus, Times, Division

object Operation {

  val parser: Parser[Operation] = Parser.oneOf(
    List(
      Parser.string("+").map(_ => Operation.Plus),
      Parser.string("-").map(_ => Operation.Minus),
      Parser.string("*").map(_ => Operation.Times),
      Parser.string("/").map(_ => Operation.Division)
    )
  )

  def apply[R: Semiring: EuclideanRing](operation: Operation): (R, R) => R =
    operation match
      case Operation.Plus     => _ + _
      case Operation.Minus    => _ - _
      case Operation.Times    => _ * _
      case Operation.Division => _.equot(_)

}
