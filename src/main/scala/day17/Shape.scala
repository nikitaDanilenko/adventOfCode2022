package day17

import utils.Pos

case class Shape(blocks: Set[Pos])

object Shape {

  enum Label:
    case HorizontalLine, Cross, Hook, VerticalLine, Block

  val all: List[Label] = Label.values.toList

  def spawn(label: Label, highestY: Int): Shape =
    val blocks = label match
      case Label.HorizontalLine =>
        Set(
          Pos(2, highestY + 3),
          Pos(3, highestY + 3),
          Pos(4, highestY + 3),
          Pos(5, highestY + 3)
        )
      case Label.Cross =>
        Set(
          Pos(3, highestY + 5),
          Pos(2, highestY + 4),
          Pos(3, highestY + 4),
          Pos(4, highestY + 4),
          Pos(3, highestY + 3)
        )
      case Label.Hook =>
        Set(
          Pos(4, highestY + 5),
          Pos(4, highestY + 4),
          Pos(2, highestY + 3),
          Pos(3, highestY + 3),
          Pos(4, highestY + 3)
        )
      case Label.VerticalLine =>
        Set(
          Pos(2, highestY + 6),
          Pos(2, highestY + 5),
          Pos(2, highestY + 4),
          Pos(2, highestY + 3)
        )
      case Label.Block =>
        Set(
          Pos(2, highestY + 4),
          Pos(3, highestY + 4),
          Pos(2, highestY + 3),
          Pos(3, highestY + 3)
        )
    Shape(blocks)

  def move(direction: Direction, shape: Shape): Shape =
    val modifier: Int => Int = direction match
      case Direction.Left  => _ - 1
      case Direction.Right => _ + 1
    shape.copy(
      blocks = shape.blocks.map(pos =>
        pos.copy(
          x = modifier(pos.x)
        )
      )
    )

  def down(shape: Shape): Shape =
    shape.copy(
      blocks = shape.blocks.map(pos =>
        pos.copy(
          y = pos.y - 1
        )
      )
    )

}
