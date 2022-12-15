package utils

import cats.Order
import spire.algebra.AdditiveGroup

case class Pos(
    x: Int,
    y: Int
)

object Pos {

  implicit val order: Order[Pos] = Order.fromLessThan { (p1, p2) =>
    p1.x < p1.x || (p1.x == p2.x && p1.y < p2.y)
  }

  implicit val additiveGroup: AdditiveGroup[Pos] = new AdditiveGroup[Pos] {
    override def zero: Pos = Pos(0, 0)

    override def plus(pos1: Pos, pos2: Pos): Pos =
      Pos(
        pos1.x + pos2.x,
        pos1.y + pos2.y
      )

    override def negate(x: Pos): Pos =
      Pos(
        -x.x,
        -x.y
      )

  }

}
