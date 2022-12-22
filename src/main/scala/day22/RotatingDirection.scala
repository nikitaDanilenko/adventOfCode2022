package day22

import cats.parse.Parser

enum RotatingDirection:
  case L, R

object RotatingDirection {

  val parser: Parser[RotatingDirection] = Parser.oneOf(
    List(
      Parser.char('L').map(_ => RotatingDirection.L),
      Parser.char('R').map(_ => RotatingDirection.R)
    )
  )

}
