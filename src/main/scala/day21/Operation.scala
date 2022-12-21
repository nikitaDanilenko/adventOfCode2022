package day21

import cats.parse.Parser

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

  def apply(operation: Operation): (BigInt, BigInt) => BigInt =
    operation match
      case Operation.Plus     => _ + _
      case Operation.Minus    => _ - _
      case Operation.Times    => _ * _
      case Operation.Division => _ / _

}
