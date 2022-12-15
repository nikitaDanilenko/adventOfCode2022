package day15

import cats.parse.Parser
import utils.{ ParserUtil, Pos }

case class Sensor(
    position: Pos,
    nearestBeacon: Pos
)

object Sensor {

  val parser: Parser[Sensor] = for {
    _ <- Parser.string("Sensor at x=")
    ownX <- ParserUtil.int
    _ <- Parser.string(", y=")
    ownY <- ParserUtil.int
    _ <- Parser.string(": closest beacon is at x=")
    beaconX <- ParserUtil.int
    _ <- Parser.string(", y=")
    beaconY <- ParserUtil.int
  } yield Sensor(
    position = Pos(
      x = ownX,
      y = ownY
    ),
    nearestBeacon = Pos(
      x = beaconX,
      y = beaconY
    )
  )

}
