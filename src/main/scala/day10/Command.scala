package day10

import cats.parse.Parser
import utils.ParserUtil

sealed trait Command

object Command {
  case class AddX(value: Int) extends Command
  case object Noop extends Command

  val parser: Parser[Command] = {
    val addXParser = for {
      _ <- Parser.string("addx ")
      value <- Parser.anyChar.rep0.map(_.mkString)
    } yield AddX(value.toInt)

    Parser.oneOf(
      List(
        Parser.string("noop").map(_ => Noop),
        addXParser
      )
    )
  }

  def toCycleInstructions(command: Command): List[CycleInstruction] = command match
    case AddX(value) => List(CycleInstruction.Wait, CycleInstruction.AddValue(value))
    case Noop        => List(CycleInstruction.Wait)

}
