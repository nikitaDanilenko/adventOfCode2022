package day17

import utils.Pos

import scala.annotation.tailrec

case class Column(
    minX: Int,
    width: Int,
    blocks: Set[Pos],
    directions: LazyList[Direction]
)

object Column {

  def fitsHorizontally(shape: Shape, column: Column): Boolean =
    shape.blocks.forall(p => p.x >= column.minX && p.x < column.minX + column.width)

  def drop(shape: Shape, column: Column): Column =
    def step(shape: Shape, column: Column): (Shape, Boolean, Column) =
      val (nextDirection, ds) = (column.directions.head, column.directions.tail)
      val horizontallyMoved =
        Some(Shape.move(nextDirection, shape))
          .filter(s => s.blocks.intersect(column.blocks).isEmpty && Column.fitsHorizontally(s, column))
          .getOrElse(shape)
      val (verticallyMoved, dropped) =
        Some(Shape.down(horizontallyMoved))
          .filter(s => s.blocks.intersect(column.blocks).isEmpty)
          .fold((horizontallyMoved, false))(_ -> true)
      (verticallyMoved, dropped, column.copy(directions = ds))

    @tailrec
    def repeat(shape: Shape, column: Column): (Shape, Column) =
      val (nextShape, dropped, nextColumn) = step(shape, column)
      if (dropped)
        repeat(nextShape, nextColumn)
      else (nextShape, nextColumn)

    val (droppedShape, newColumn) = repeat(shape, column)
    newColumn.copy(
      blocks = newColumn.blocks ++ droppedShape.blocks
    )

  def dropMany(shapes: LazyList[Shape.Label], column: Column): LazyList[Column] =
    val (label, nextShapes) = (shapes.head, shapes.tail)
    val topY = column.blocks.map(_.y).max
    val shape = Shape.spawn(
      label = label,
      leftX = column.minX,
      topY = topY
    )
    val nextColumn = drop(shape, column)
    column #:: dropMany(nextShapes, nextColumn)

}
