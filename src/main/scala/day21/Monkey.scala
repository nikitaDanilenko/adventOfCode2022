package day21

import cats.parse.Parser
import utils.ParserUtil

sealed trait Monkey {
  def name: String
}

object Monkey {
  case class Number(override val name: String, bigInt: BigInt) extends Monkey

  case class Computation(override val name: String, operand1: String, operand2: String, operation: Operation)
      extends Monkey

  val parser: Parser[Monkey] = {
    val nameParser = Parser.charIn('a'.to('z')).rep.map(_.toList.mkString)
    val plainParser = for {
      name <- nameParser
      _ <- Parser.string(": ")
      int <- ParserUtil.int
    } yield Number(
      name,
      BigInt(int)
    )
    val computationParser = for {
      name <- nameParser
      _ <- Parser.string(": ")
      operand1 <- nameParser
      _ <- Parser.char(' ')
      operation <- Operation.parser
      _ <- Parser.char(' ')
      operand2 <- nameParser
    } yield Computation(
      name = name,
      operand1 = operand1,
      operand2 = operand2,
      operation = operation
    )
    Parser.oneOf(
      List(
        plainParser.backtrack,
        computationParser
      )
    )
  }

}
