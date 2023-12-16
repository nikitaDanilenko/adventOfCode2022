package day24

import scala.io.Source

object Day24 {

  case class Blizzard(
      startingPosition: Position,
      direction: Direction
  )

  case class Valley(
      blizzards: List[Blizzard],
      height: Int,
      width: Int
  )

  def blizzardAtTime(
      timeUnit: Int,
      blizzard: Blizzard,
      height: Int,
      width: Int
  ): Position =
    Direction.move(
      direction = blizzard.direction,
      position = blizzard.startingPosition,
      units = timeUnit,
      height = height,
      width = width
    )

  def shortestPath(
      sourcePosition: Position,
      targetPosition: Position,
      valley: Valley
  ): Int = {

    def bfs(current: Set[Position], step: Int): LazyList[Set[Position]] = {
      val blizzardPositions =
        valley.blizzards
          .map(blizzard =>
            blizzardAtTime(
              timeUnit = 1 + step,
              blizzard = blizzard,
              height = valley.height,
              width = valley.width
            )
          )
          .toSet
      val next = current.flatMap { position =>
        val neighbours =
          Position
            .neighbours(
              position = position,
              width = valley.width,
              height = valley.height
            )
            .diff(blizzardPositions)
        neighbours
      }
      current #:: bfs(next, step + 1)
    }

    bfs(Set(sourcePosition), 0).zipWithIndex
      .dropWhile(!_._1.contains(targetPosition))
      .headOption
      .map(_._2)
      .get
  }

  val input: Valley = {
    val lines = Source
      .fromResource("day24.txt")
      .getLines()
      .toList
      .tail
      .dropRight(1)
      .map(_.filter(_ != '#'))

    val blizzards = lines.zipWithIndex.flatMap { case (line, lineIndex) =>
      line.zipWithIndex.flatMap { case (char, columnIndex) =>
        Direction.parseDirection(char).map { direction =>
          Blizzard(Position(lineIndex, columnIndex), direction)
        }
      }
    }
    Valley(
      blizzards = blizzards,
      height = lines.length,
      width = lines.head.length
    )
  }

  def prettyPrint(valley: Valley): Unit = {
    val lines = (0 until valley.height).map { y =>
      (0 until valley.width).map { x =>
        valley.blizzards.find(_.startingPosition == Position(x, y)).fold('.') { blizzard =>
          blizzard.direction match {
            case Direction.North => '^'
            case Direction.South => 'v'
            case Direction.West  => '<'
            case Direction.East  => '>'
          }
        }
      }.mkString
    }
    println(lines.mkString("\n"))
  }

  @main
  def solution1(): Unit =
    val pathLength = shortestPath(Position(-1, 0), Position(input.height, input.width - 1), input)
    println(pathLength)

}
