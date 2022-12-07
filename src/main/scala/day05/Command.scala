package day05

import cats.parse.Parser
import cats.parse.Rfc5234.*
import utils.ParserUtil

case class Command(
    amount: Int,
    from: Int,
    to: Int
)

object Command {

  val parser: Parser[Command] =
    for {
      _ <- Parser.string("move ")
      amount <- ParserUtil.int
      _ <- Parser.string(" from ")
      from <- ParserUtil.int
      _ <- Parser.string(" to ")
      to <- ParserUtil.int

    } yield Command(
      amount = amount,
      from = from,
      to = to
    )

}
