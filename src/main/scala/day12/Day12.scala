package day12

import utils.Graph

import scala.io.Source
import scala.util.chaining.*

object Day12 {

  val valueMap: Map[Char, Int] =
    'a'
      .to('z')
      .zipWithIndex
      .toMap
      .updated('S', 0)
      .updated('E', 25)

  type Pos = (Int, Int)

  case class Input(
      graph: Graph[Pos],
      start: Pos,
      end: Pos,
      lowest: Set[Pos]
  )

  val input: Input = {
    val nodeMap = Source
      .fromResource("real/day12.txt")
      .getLines()
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (indexedLine, i) =>
        indexedLine.map { case (c, j) =>
          (1 + i, 1 + j) -> c
        }
      }
      .toMap

    def cross(pos: (Int, Int), value: Char): List[(Int, Int)] =
      val (i, j) = pos
      List(
        (i - 1, j),
        (i + 1, j),
        (i, j - 1),
        (i, j + 1)
      ).filter { pos =>
        nodeMap
          .get(pos)
          .exists { neighbourValue =>
            val atPos = valueMap(value)
            val atNeighbour = valueMap(neighbourValue)
            atNeighbour <= atPos || atPos + 1 == atNeighbour
          }
      }

    val start = nodeMap.find(_._2 == 'S').get._1
    val end = nodeMap.find(_._2 == 'E').get._1

    val lowest = nodeMap.filter { case (_, c) => c == 'S' || c == 'a' }.keySet

    Input(
      graph = Graph(
        nodeMap.map { case (pos, c) =>
          pos -> cross(pos, c)
        }
      ),
      start = start,
      end = end,
      lowest = lowest
    )
  }

  def shortestPathFromTo(
      start: Set[Pos],
      target: Set[Pos]
  ): Option[Int] =
    val steps = Graph.layers(
      from = start,
      graph = input.graph
    )
    val (path, rest) =
      steps.map(_.intersect(target)).span(_.isEmpty)

    Some(path.length).filter(_ => rest.headOption.nonEmpty)

  @main
  def solution1(): Unit =
    val shortest = shortestPathFromTo(
      start = Set(input.start),
      target = Set(input.end)
    )

    shortest.fold(pprint.log("No path found"))(pprint.log(_))

  @main
  def solution2(): Unit =
    val shortest = shortestPathFromTo(
      start = input.lowest,
      target = Set(input.end)
    )

    shortest.fold(pprint.log("No path found"))(pprint.log(_))

}
