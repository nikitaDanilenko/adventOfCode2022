package day17

import cats.parse.Parser

enum Direction:
  case Left, Right

object Direction {

  val parser: Parser[Direction] = Parser.oneOf(
    List(
      Parser.char('<').map(_ => Direction.Left),
      Parser.char('>').map(_ => Direction.Right)
    )
  )

}
