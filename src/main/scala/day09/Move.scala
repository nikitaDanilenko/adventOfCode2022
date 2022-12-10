package day09

import cats.parse.Parser
import utils.ParserUtil

case class Move(
    direction: Direction,
    units: Int
)

object Move {

  val parser: Parser[Move] = for {
    direction <- Direction.parser
    _ <- Parser.char(' ')
    units <- ParserUtil.positiveInt
  } yield Move(direction, units)

  def unfold(move: Move): List[Move] =
    List.fill(move.units)(move.copy(units = 1))

}
