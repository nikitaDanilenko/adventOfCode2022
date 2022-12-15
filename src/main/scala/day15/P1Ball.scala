package day15

import utils.Pos

case class P1Ball(
    centre: Pos,
    radius: Int
)

object P1Ball {

  def distance(pos1: Pos, pos2: Pos): Int =
    (pos1.x - pos2.x).abs + (pos1.y - pos2.y).abs

  def intersectAtY(y: Int, p1Ball: P1Ball): Interval =
    val base = (y - p1Ball.centre.y).abs - p1Ball.radius
    Interval.NonEmpty(base + p1Ball.centre.x, -base + p1Ball.centre.x)

}
