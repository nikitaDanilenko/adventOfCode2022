package day17

import utils.Pos

import scala.io.Source

object Day17 {

  val input: List[Direction] = Source
    .fromResource("real/day17.txt")
    .flatMap(c => Direction.parser.parse(s"$c").toOption.map(_._2))
    .toList

  val initialColumn: Column = {
    val width = 7
    Column(
      minX = 0,
      width = width,
      blocks = 0.until(width).map(Pos(_, 0)).toSet,
      directions = LazyList.continually(input).flatten
    )
  }

  def visualize(column: Column): String =
    0.to(column.blocks.map(_.y).max + 3)
      .reverse
      .map { y =>
        column.minX
          .until(column.minX + column.width)
          .map { x =>
            val pos = Pos(x, y)
            if (column.blocks.contains(pos)) '#' else '.'
          }
          .mkString
      }
      .mkString("\n")

  @main
  def solution1(): Unit =
    val droppedMany = Column.dropMany(
      LazyList.continually(Shape.all).flatten,
      initialColumn
    )

    val finalColumn = droppedMany.apply(2022)

    val height = finalColumn.blocks.map(_.y).max
    pprint.log(height)

}
