package day16

import cats.parse.Parser
import utils.{ Graph, ParserUtil }
import scala.util.chaining._

import scala.io.Source

object Day16 {

  case class ValveInfo(
      name: String,
      flowRate: Int,
      neighbours: List[String]
  )

  object ValveInfo {

    val parser: Parser[ValveInfo] =
      val nameParser = for {
        c1 <- Parser.anyChar
        c2 <- Parser.anyChar
      } yield List(c1, c2).mkString

      for {
        _ <- Parser.string("Valve ")
        name <- nameParser
        _ <- Parser.string(" has flow rate=")
        flowRate <- ParserUtil.int
        _ <- Parser.oneOf(
          List(
            Parser.string("; tunnels lead to valves "),
            Parser.string("; tunnel leads to valve ")
          )
        )
        neighbours <- nameParser.repSep(Parser.string(", "))
      } yield ValveInfo(
        name = name,
        flowRate = flowRate,
        neighbours = neighbours.toList
      )

  }

  val input: List[ValveInfo] =
    Source
      .fromResource("test/day16.txt")
      .getLines()
      .flatMap(ValveInfo.parser.parse(_).toOption.map(_._2))
      .toList

  def mkGraph(valveInfos: List[ValveInfo]): Graph[String] =
    valveInfos
      .map(vi => vi.name -> vi.neighbours)
      .toMap
      .pipe(Graph(_))

  @main
  def solution1(): Unit =
    val graph = mkGraph(input)

    pprint.log(input)
    val shortest = Graph.shortestPaths(graph)
    pprint.log(shortest)

}
