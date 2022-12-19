package day19

import cats.parse.Parser
import utils.ParserUtil

import java.util.concurrent.CompletionStage

case class Blueprint(
    id: Int,
    oreBot: Costs,
    clayBot: Costs,
    obsidianBot: Costs,
    geodeBot: Costs
)

object Blueprint {

  val parser: Parser[Blueprint] = for {
    _ <- Parser.string("Blueprint ")
    id <- ParserUtil.positiveInt
    _ <- Parser.string(": Each ore robot costs ")
    oreBot <- Costs.parser
    _ <- Parser.string(". Each clay robot costs ")
    clayBot <- Costs.parser
    _ <- Parser.string(". Each obsidian robot costs ")
    obsidianBot <- Costs.parser
    _ <- Parser.string(". Each geode robot costs ")
    geodeBot <- Costs.parser
    _ <- Parser.char('.')
  } yield Blueprint(
    id = id,
    oreBot = oreBot,
    clayBot = clayBot,
    obsidianBot = obsidianBot,
    geodeBot = geodeBot
  )

}
