package day24

enum Direction {
  case North, South, East, West
}

object Direction {

  def parseDirection(c: Char): Option[Direction] = c match {
    case '^' => Some(North)
    case 'v' => Some(South)
    case '>' => Some(East)
    case '<' => Some(West)
    case _   => None
  }

  def move(
      direction: Direction,
      position: Position,
      units: Int,
      height: Int,
      width: Int
  ): Position = {
    val unitsModHeight = units % height
    val unitsModWidth = units  % width
    direction match {
      // TODO: There is still some overflow issue here.
      case Direction.North =>
        val newLine =
          if (unitsModHeight <= position.line) position.line - unitsModHeight
          else height - (unitsModHeight - position.line)
        position.copy(line = newLine)
      case Direction.South =>
        val newLine =
          if (unitsModHeight + position.line < height) position.line + unitsModHeight
          else unitsModHeight - (height - position.line)
        position.copy(line = newLine)
      case Direction.East =>
        val newColumn =
          if (unitsModWidth + position.column < width) position.column + unitsModWidth
          else unitsModWidth - (width - position.column)
        position.copy(column = newColumn)
      case Direction.West =>
        val newColumn =
          if (unitsModWidth <= position.column) position.column - unitsModWidth
          else width - (unitsModWidth - position.column)
        position.copy(column = newColumn)

    }
  }

}
