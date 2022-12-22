package day22

import cats.parse.Parser

enum AreaType:
  case Free, Wall

object AreaType {

  val parser: Parser[AreaType] = Parser.oneOf(
    List(
      Parser.char('.').map(_ => AreaType.Free),
      Parser.char('#').map(_ => AreaType.Wall)
    )
  )

}
