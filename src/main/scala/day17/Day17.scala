package day17

import cats.data.State
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
    pprint.log(finalColumn.directionsUsed)
    pprint.log(height)

  @main
  def solution2(): Unit =
    val droppedMany = Column.dropMany(
      LazyList.continually(Shape.all).flatten,
      initialColumn
    )

    // Surface profile alone is not sufficient, because a block may be moved in a C shape
    // (just) under the surface, and thus the profile stays the same.
    // The additional Int is the number of used instructions modulo the total number of instructions.
    // The cycle may not be the shortest possible one.
    type SurfaceProfiles = Map[(Set[Pos], Int), Int]

    def findCycle(lazyList: LazyList[(Column, Int)]): State[SurfaceProfiles, (Int, Int)] =
      val (column, index) = lazyList.head
      for {
        surfaceProfiles <- State.get[SurfaceProfiles]
        surfaceProfile = Column.surfaceProfile(column)
        result <- surfaceProfiles
          .get((surfaceProfile, column.directionsUsed % input.length))
          .fold {
            for {
              _ <- State.modify[SurfaceProfiles](
                _.updated((surfaceProfile, column.directionsUsed % input.length), index)
              )
              next <- findCycle(lazyList.tail)
            } yield next
          }(first => State.pure((first, index)))
      } yield result

    val cycle = findCycle(droppedMany.zipWithIndex).run(Map.empty).value._2
    pprint.log(cycle)

    val heights: Int => Int = droppedMany(_).blocks.map(_.y).max
    val targetShapes = 1000000000000L

    val cycleStart = cycle._1
    val cycleLength = cycle._2 - cycle._1
    val cycleHeight = heights(cycle._2) - heights(cycle._1)
    val cyclicShapes = targetShapes - cycleStart
    val div = cyclicShapes / cycleLength
    val mod = (cyclicShapes % cycleLength).intValue()
    /* intuitively one has:
       heights(cycleStart) + div * cycleHeight + (heights(cycleStart + mod) - heights(cycleStart)),
       but heights(cycleStart) cancels out.
     */
    val height = div * cycleHeight + heights(cycleStart + mod)
    pprint.log(height)

}
