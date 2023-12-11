package day23

import scala.io.Source

object Day23 {

  case class Position(
      row: Int,
      column: Int
  )

  type Grove = Set[Position]

  private val input: Grove =
    Source
      .fromResource("day23.txt")
      .getLines
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.zipWithIndex
          .collect { case (c, column) if c == '#' => Position(row, column) }
      }
      .toSet

  enum Rule:
    case North, South, West, East;

  private def north(position: Position): Position =
    position.copy(row = position.row - 1)

  private def south(position: Position): Position =
    position.copy(row = position.row + 1)

  private def west(position: Position): Position =
    position.copy(column = position.column - 1)

  private def east(position: Position): Position =
    position.copy(column = position.column + 1)

  private def northWest(position: Position): Position =
    Position(row = position.row - 1, column = position.column - 1)

  private def northEast(position: Position): Position =
    Position(row = position.row - 1, column = position.column + 1)

  private def southWest(position: Position): Position =
    Position(row = position.row + 1, column = position.column - 1)

  private def southEast(position: Position): Position =
    Position(row = position.row + 1, column = position.column + 1)

  def neighboursOf(position: Position, rule: Rule): Set[Position] =
    rule match {
      case Rule.North => Set(north(position), northWest(position), northEast(position))
      case Rule.South => Set(south(position), southWest(position), southEast(position))
      case Rule.West  => Set(west(position), northWest(position), southWest(position))
      case Rule.East  => Set(east(position), northEast(position), southEast(position))
    }

  def proposeByRule(rule: Rule): Position => Position = rule match
    case Rule.North => north
    case Rule.South => south
    case Rule.West  => west
    case Rule.East  => east

  case class Proposition(
      elfPosition: Position,
      proposedPosition: Position
  )

  def allNeighboursOf(position: Position): Set[Position] =
    Set(
      north,
      south,
      west,
      east,
      northWest,
      northEast,
      southWest,
      southEast
    ).map(_(position))

  def propose(
      grove: Grove,
      rules: List[Rule]
  ): Set[Proposition] =
    grove.map { elfPosition =>
      val allNeighbours = allNeighboursOf(elfPosition)
      if allNeighbours.intersect(grove).isEmpty then
        Proposition(
          elfPosition = elfPosition,
          proposedPosition = elfPosition
        )
      else
        rules
          .collectFirst {
            case rule if neighboursOf(elfPosition, rule).intersect(grove).isEmpty =>
              Proposition(
                elfPosition = elfPosition,
                proposedPosition = proposeByRule(rule)(elfPosition)
              )
          }
          .getOrElse(Proposition(elfPosition, elfPosition))
    }

  def move(
      propositions: Iterable[Proposition]
  ): Grove = {
    val targetPositions = propositions.groupBy(_.proposedPosition)
    propositions.map { proposition =>
      if (targetPositions(proposition.proposedPosition).size > 1) proposition.elfPosition
      else proposition.proposedPosition
    }.toSet
  }

  case class GroveTransition(
      grove: Grove,
      rules: List[Rule]
  )

  def rotate[A](list: List[A]): List[A] = if list.isEmpty then list else list.tail :+ list.head

  def step(groveTransition: GroveTransition): GroveTransition =
    GroveTransition(
      move(propose(groveTransition.grove, groveTransition.rules)),
      rotate(groveTransition.rules)
    )

  def boundingAreaSize(iterable: Iterable[Position]): Int =
    val minRow = iterable.minBy(_.row).row
    val maxRow = iterable.maxBy(_.row).row
    val minColumn = iterable.minBy(_.column).column
    val maxColumn = iterable.maxBy(_.column).column
    (maxRow - minRow + 1) * (maxColumn - minColumn + 1)

  val startRules: List[Rule] = List(Rule.North, Rule.South, Rule.West, Rule.East)

  @main
  def solution1(): Unit =
    val finalGrove =
      List
        .iterate(GroveTransition(input, startRules), 11)(step)
        .last
        .grove
    val boundingArea = boundingAreaSize(finalGrove)
    val empty = boundingArea - finalGrove.size
    println(s"Solution 1: $empty")

  @main
  def solution2(): Unit =
    val allSteps = LazyList.iterate(GroveTransition(input, startRules))(step)
    val result = allSteps.zip(allSteps.tail).zipWithIndex.collectFirst {
      case ((previous, next), index) if previous.grove == next.grove =>
        1 + index
    }
    println(s"Solution2: ${result.get}")

}
