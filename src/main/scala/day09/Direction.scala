package day09

import cats.parse.Parser

sealed trait Direction

object Direction {
  case object L extends Direction
  case object R extends Direction
  case object U extends Direction
  case object D extends Direction

  val parser: Parser[Direction] = Parser.oneOf(
    List(
      Parser.char('L').map(_ => L),
      Parser.char('R').map(_ => R),
      Parser.char('U').map(_ => U),
      Parser.char('D').map(_ => D)
    )
  )

}
