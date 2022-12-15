package utils

import cats.parse.Parser
import cats.parse.Rfc5234._

object ParserUtil {

  val positiveInt: Parser[Int] =
    digit
      .rep(1)
      .map(_.toList.mkString.toInt)

  val int: Parser[Int] = {
    val negative = for {
      _ <- Parser.char('-')
      p <- positiveInt
    } yield -p
    Parser.oneOf(
      List(
        positiveInt,
        negative
      )
    )
  }

}
