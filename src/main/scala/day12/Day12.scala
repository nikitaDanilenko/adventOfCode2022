package day12

import scala.io.Source
import scala.util.chaining._

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
      end: Pos
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

    Input(
      graph = Graph(
        nodeMap.map { case (pos, c) =>
          pos -> cross(pos, c)
        }
      ),
      start = start,
      end = end
    )
  }

  @main
  def solution1(): Unit =
    val steps = Graph.layers(
      from = Set(input.start),
      graph = input.graph
    )
    val (path, rest) =
      steps.span(!_.contains(input.end))

    if (rest.headOption.exists(_.contains(input.end)))
      pprint.log(path.length)
    else
      pprint.log("No path found")

}
