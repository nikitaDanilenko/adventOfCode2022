package day19

import cats.parse.Parser
import utils.ParserUtil
import scala.util.chaining._

case class Costs(
    resources: Map[Resource, Int]
)

object Costs {

  val parser: Parser[Costs] = {
    val singleParser: Parser[(Resource, Int)] = for {
      int <- ParserUtil.positiveInt
      _ <- Parser.char(' ')
      resource <- Resource.parser
    } yield (resource, int)

    singleParser.repSep(Parser.string(" and ")).map(_.toList.toMap.pipe(Costs.apply))
  }

}
