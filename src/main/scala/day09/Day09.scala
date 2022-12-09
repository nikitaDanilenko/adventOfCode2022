package day09

import spire.algebra.{ AdditiveGroup, AdditiveMonoid }

import scala.io.Source
import spire.implicits.*
import scala.util.chaining._

object Day09 {

  val input: Iterator[Move] = Source
    .fromResource("day09.txt")
    .getLines()
    .flatMap(Move.parser.parse(_).toOption.map(_._2))

  case class Pos(
      x: Int,
      y: Int
  )

  object Pos {

    implicit val monoid: AdditiveGroup[Pos] = new AdditiveGroup[Pos] {
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

  def moveToHead(head: Pos, tail: Pos): Pos =
    val diff = head - tail
    val move =
      if (diff.x.abs <= 1 && diff.y.abs <= 1)
        AdditiveGroup[Pos].zero
      else if (diff == Pos(0, 2))
        Pos(0, 1)
      else if (diff == Pos(0, -2))
        Pos(0, -1)
      else if (diff == Pos(2, 0))
        Pos(1, 0)
      else if (diff == Pos(-2, 0))
        Pos(-1, 0)
      else if (diff == Pos(1, 2) || diff == Pos(2, 1))
        Pos(1, 1)
      else if (diff == Pos(2, -1) || diff == Pos(1, -2))
        Pos(1, -1)
      else if (diff == Pos(-1, -2) || diff == Pos(-2, -1))
        Pos(-1, -1)
      else
        Pos(-1, 1)
    tail + move

  def movePos(pos: Pos, move: Move): Pos =
    val step =
      move.direction match
        case Direction.L => Pos(-move.units, 0)
        case Direction.R => Pos(move.units, 0)
        case Direction.U => Pos(0, move.units)
        case Direction.D => Pos(0, -move.units)
    step + pos

  def moveAll(moves: Iterable[Move]): Int =
    val (_, _, finalVisited) =
      moves.foldLeft((AdditiveGroup[Pos].zero, AdditiveGroup[Pos].zero, Set(AdditiveGroup[Pos].zero))) {
        case ((head, tail, visited), move) =>
          val movedHead = movePos(head, move)
          val movedTail = moveToHead(movedHead, tail)
          (movedHead, movedTail, visited + movedTail)
      }
    finalVisited.size

  @main
  def solution1(): Unit =
    input
      .flatMap(Move.unfold)
      .toList
      .pipe(moveAll)
      .pipe(pprint.log(_))

}
