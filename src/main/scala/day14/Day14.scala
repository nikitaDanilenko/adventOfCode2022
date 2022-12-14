package day14

import cats.parse.{ Parser, Parser0 }
import utils.ParserUtil

import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  def positions(from: Pos, to: Pos): List[Pos] =
    if (from.x == to.x)
      math
        .min(from.y, to.y)
        .to(math.max(from.y, to.y))
        .map(Pos(from.x, _))
        .toList
    else if (from.y == to.y)
      math
        .min(from.x, to.x)
        .to(math.max(from.x, to.x))
        .map(Pos(_, from.y))
        .toList
    else List.empty

  val positionsParser: Parser0[List[Pos]] =
    val pos: Parser[Pos] = for {
      x <- ParserUtil.positiveInt
      _ <- Parser.char(',')
      y <- ParserUtil.positiveInt
    } yield Pos(x, y)

    pos.repSep0(Parser.string(" -> "))

  val input: Set[Pos] =
    Source
      .fromResource("real/day14.txt")
      .getLines()
      .flatMap(positionsParser.parse(_).toOption.map(_._2))
      .flatMap { list =>
        list.zip(list.tail).flatMap { case (from, to) =>
          positions(from, to)
        }
      }
      .toSet

  sealed trait Movement

  object Movement {
    case object Out extends Movement
    case class Falling(pos: Pos) extends Movement
    case class Settled(pos: Pos) extends Movement
  }

  def trickle(structure: Set[Pos]): Set[Pos] =
    val xs = structure.map(_.x)
    val xMin = xs.min
    val xMax = xs.max
    val yMax = structure.map(_.y).max

    def inArea(pos: Pos): Boolean =
      pos.x >= xMin && pos.x <= xMax && pos.y <= yMax

    def descend(current: Pos, filled: Set[Pos]): Movement =
      val down = current.copy(y = current.y + 1)
      val diaLeft = current.copy(x = current.x - 1, y = current.y + 1)
      val diaRight = current.copy(x = current.x + 1, y = current.y + 1)

      List(down, diaLeft, diaRight)
        .collectFirst { case pos if !filled.contains(pos) => pos }
        .fold(Movement.Settled(current): Movement) { other =>
          if inArea(other) then Movement.Falling(other)
          else Movement.Out
        }

    val initial = Pos(500, 0)

    def settle(filled: Set[Pos]): Set[Pos] =
      val result = LazyList.iterate(Movement.Falling(initial): Movement, 1 + yMax) {
        case Movement.Falling(pos) => descend(pos, filled)
        case x                     => x
      }
      val next = result
        .dropWhile {
          case Movement.Settled(_) => false
          case _                   => true
        }
        .headOption
        .collect { case Movement.Settled(pos) => pos }

      filled ++ next

    @tailrec
    def repeat(structure: Set[Pos]): Set[Pos] =
      val next = settle(structure)
      if (next == structure)
        structure
      else repeat(next)

    repeat(structure)

  def draw(structure: Set[Pos]): Unit =
    val string = for {
      y <- 0.to(9)
    } yield 494
      .to(503)
      .map { x =>
        val pos = Pos(x, y)
        if input.contains(pos) then '#'
        else if structure.contains(pos) then 'o'
        else '.'
      }
      .mkString
    println(string.mkString("\n"))

  @main
  def solution1(): Unit =
    val grains = trickle(input).size - input.size
    pprint.log(grains)

}
