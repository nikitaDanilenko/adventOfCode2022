package utils

import cats.parse.Parser
import cats.parse.Rfc5234._

object ParserUtil {

  val int: Parser[Int] =
    digit
      .rep(1)
      .map(_.toList.mkString.toInt)

}
