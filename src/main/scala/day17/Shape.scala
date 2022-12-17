package day17

import utils.Pos

import scala.annotation.tailrec

case class Shape(blocks: Set[Pos])

object Shape {

  enum Label:
    case HorizontalLine, Cross, Hook, VerticalLine, Block

  val all: List[Label] = Label.values.toList

  def spawn(label: Label, leftX: Int, topY: Int): Shape =
    val blocks = label match
      case Label.HorizontalLine =>
        Set(
          Pos(leftX + 2, topY + 4),
          Pos(leftX + 3, topY + 4),
          Pos(leftX + 4, topY + 4),
          Pos(leftX + 5, topY + 4)
        )
      case Label.Cross =>
        Set(
          Pos(leftX + 3, topY + 6),
          Pos(leftX + 2, topY + 5),
          Pos(leftX + 3, topY + 5),
          Pos(leftX + 4, topY + 5),
          Pos(leftX + 3, topY + 4)
        )
      case Label.Hook =>
        Set(
          Pos(leftX + 4, topY + 6),
          Pos(leftX + 4, topY + 5),
          Pos(leftX + 2, topY + 4),
          Pos(leftX + 3, topY + 4),
          Pos(leftX + 4, topY + 4)
        )
      case Label.VerticalLine =>
        Set(
          Pos(leftX + 2, topY + 7),
          Pos(leftX + 2, topY + 6),
          Pos(leftX + 2, topY + 5),
          Pos(leftX + 2, topY + 4)
        )
      case Label.Block =>
        Set(
          Pos(leftX + 2, topY + 5),
          Pos(leftX + 3, topY + 5),
          Pos(leftX + 2, topY + 4),
          Pos(leftX + 3, topY + 4)
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
