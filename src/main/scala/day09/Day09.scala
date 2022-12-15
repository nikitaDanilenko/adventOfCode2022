package day09

import spire.algebra.{ AdditiveGroup, AdditiveMonoid }

import scala.io.Source
import spire.implicits.*
import utils.Pos

import scala.util.chaining.*

object Day09 {

  val input: Iterator[Move] = Source
    .fromResource("real/day09.txt")
    .getLines()
    .flatMap(Move.parser.parse(_).toOption.map(_._2))

  def moveToHead(head: Pos, tail: Pos): Pos =
    val diff = head - tail
    val isTopRight = diff.x > 0 && diff.y > 0
    val isTopLeft = diff.x < 0 && diff.y > 0
    val isBottomRight = diff.x > 0 && diff.y < 0
    val isBottomLeft = diff.x < 0 && diff.y < 0
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
      else if (isTopRight)
        Pos(1, 1)
      else if (isBottomRight)
        Pos(1, -1)
      else if (isBottomLeft)
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

  def moveAll1(moves: Iterable[Move]): Int =
    val (_, _, finalVisited) =
      moves.foldLeft((AdditiveGroup[Pos].zero, AdditiveGroup[Pos].zero, Set(AdditiveGroup[Pos].zero))) {
        case ((head, tail, visited), move) =>
          val movedHead = movePos(head, move)
          val movedTail = moveToHead(movedHead, tail)
          (movedHead, movedTail, visited + movedTail)
      }
    finalVisited.size

  def moveAll10(moves: Iterable[Move]): Int =
    val zero = AdditiveGroup[Pos].zero
    val (_, _, finalVisited) =
      moves.foldLeft(zero, List.fill(9)(zero), Set(zero)) { case ((h, ts, visited), move) =>
        val movedHead = movePos(h, move)
        val movedTs = ts
          .foldLeft((List.empty[Pos], movedHead)) { case ((list, moved), next) =>
            val movedNext = moveToHead(moved, next)
            (list :+ movedNext, movedNext)
          }
          ._1

        (
          movedHead,
          movedTs,
          visited + movedTs.last
        )
      }
    finalVisited.size

  @main
  def solution1(): Unit =
    input
      .flatMap(Move.unfold)
      .toList
      .pipe(moveAll1)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    input
      .flatMap(Move.unfold)
      .toList
      .pipe(moveAll10)
      .pipe(pprint.log(_))

}
