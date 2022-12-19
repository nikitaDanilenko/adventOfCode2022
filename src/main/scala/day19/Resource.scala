package day19

import cats.parse.Parser

enum Resource:
  case Ore, Clay, Obsidian, Geode

object Resource {

  val parser: Parser[Resource] = Parser.oneOf(
    List(
      Parser.string("ore").map(_ => Resource.Ore),
      Parser.string("clay").map(_ => Resource.Clay),
      Parser.string("obsidian").map(_ => Resource.Obsidian),
      Parser.string("geode").map(_ => Resource.Geode)
    )
  )

}
