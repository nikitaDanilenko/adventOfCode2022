package day24

case class Position(
    line: Int,
    column: Int
)

object Position {

  def neighbours(position: Position, width: Int, height: Int): Set[Position] =
    if (position == Position(-1, 0))
      // It may be necessary to wait before the first step
      Set(position, Position(0, 0))
    else if (position == Position(height - 1, width - 1))
      // There should be no need to go back again
      Set(position, Position(height, width - 1))
    else
      Set(
        position,
        position.copy(line = position.line - 1),
        position.copy(line = position.line + 1),
        position.copy(column = position.column - 1),
        position.copy(column = position.column + 1)
      ).filter(p => p.line >= 0 && p.line < height && p.column >= 0 && p.column < width)

}
