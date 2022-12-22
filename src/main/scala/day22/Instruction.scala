package day22

import cats.parse.Parser
import utils.ParserUtil

sealed trait Instruction

object Instruction {

  case class Move(units: Int) extends Instruction
  case class Rotate(rotatingDirection: RotatingDirection) extends Instruction

  val parser: Parser[Instruction] =
    Parser.oneOf(
      List(
        ParserUtil.int.map(Move.apply),
        RotatingDirection.parser.map(Rotate.apply)
      )
    )

}
