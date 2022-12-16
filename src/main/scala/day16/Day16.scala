package day16

import cats.parse.Parser
import utils.{ CollectionUtil, Graph, ParserUtil }

import scala.annotation.tailrec
import scala.util.chaining.*
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
      .fromResource("real/day16.txt")
      .getLines()
      .flatMap(ValveInfo.parser.parse(_).toOption.map(_._2))
      .toList

  def mkGraph(valveInfos: List[ValveInfo]): Graph[String] =
    valveInfos
      .map(vi => vi.name -> vi.neighbours)
      .toMap
      .pipe(Graph(_))

  // largely based on https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcode2022/Day16.scala
  def findMaxPressure(valveInfos: List[ValveInfo], maxMinutes: Int): Map[Set[String], Int] = {
    val distances = Graph
      .shortestPaths(mkGraph(input))
      .perNode
      .view
      .mapValues {
        _.collect { case (name, Graph.Tropical.Value(v)) => name -> v }
      }

    val valveMap = valveInfos.map(vi => vi.name -> vi).toMap
    val nonZeroValves = valveInfos.collect { case vi if vi.flowRate > 0 => vi.name }.toSet

    case class State(
        atValve: String,
        openValves: Set[String]
    )

    // First Int = Minutes, second Int = Pressure
    type MinuteMap = Map[Int, Map[State, Int]]

    def maxUnion[A](map1: Map[A, Int], map2: Map[A, Int]): Map[A, Int] = CollectionUtil.unionWith(map1, map2)(math.max)

    @tailrec
    def iterate(
        passedMinutes: Int,
        minutesMap: MinuteMap
    ): Map[Set[String], Int] =
      if (passedMinutes < maxMinutes) {
        val newOpenValvesList = for {
          (state, pressure) <- minutesMap.getOrElse(passedMinutes, Map.empty).toList
          nextValve <- nonZeroValves.diff(state.openValves)
          distance <- distances(state.atValve).get(nextValve).toList
          finishedAtMinutes <- Some(passedMinutes + distance + 1) // +1 since opening takes a minute
            .filter(_ < maxMinutes)
            .toList
        } yield finishedAtMinutes ->
          Map(
            State(
              nextValve,
              state.openValves + nextValve
            ) ->
              (pressure + valveMap(nextValve).flowRate * (maxMinutes - finishedAtMinutes))
          )

        // Minutes may occur multiple times, which is why it is necessary to compute a new map properly
        val newOpenValves = newOpenValvesList
          .groupBy(_._1)
          .view
          .mapValues(
            _.map(_._2)
              .foldLeft(Map.empty[State, Int])(maxUnion)
          )
          .toMap

        val newMinutesMap = CollectionUtil.unionWith(minutesMap, newOpenValves)(maxUnion)
        iterate(1 + passedMinutes, newMinutesMap)
      } else {
        minutesMap.values
          .flatMap(_.map { case (state, pressure) => state.openValves -> pressure })
          .map(Map(_))
          .foldLeft(Map.empty[Set[String], Int])(maxUnion)
      }

    iterate(0, Map(0 -> Map(State("AA", Set.empty) -> 0)))
  }

  @main
  def solution1(): Unit =
    val max = findMaxPressure(input, 30)
    pprint.log(max.values.max)

  @main
  def solution2(): Unit =
    val pressures = findMaxPressure(input, 26).toVector
    val combined = for {
      ((valves1, pressure1), i) <- pressures.zipWithIndex
      (_, pressure2) <- pressures.drop(1 + i).filter(_._1.intersect(valves1).isEmpty)
    } yield pressure1 + pressure2

    val max = combined.max

    pprint.log(max)

}
