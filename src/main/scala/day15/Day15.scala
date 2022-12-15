package day15

import cats.Order
import utils.Pos

import scala.io.Source

object Day15 {

  val input: List[Sensor] =
    Source
      .fromResource("real/day15.txt")
      .getLines()
      .flatMap(Sensor.parser.parse(_).toOption.map(_._2))
      .toList

  case class BallAndBeacon(
      ball: P1Ball,
      beacon: Pos
  )

  private val ballsAndBeacons: List[BallAndBeacon] = input.map { sensor =>
    BallAndBeacon(
      ball = P1Ball(
        sensor.position,
        P1Ball.distance(sensor.position, sensor.nearestBeacon)
      ),
      beacon = sensor.nearestBeacon
    )
  }

  def intersectAllWithY(targetY: Int): List[Interval] =
    val intervals = ballsAndBeacons
      .map(ballAndBeacon => P1Ball.intersectAtY(targetY, ballAndBeacon.ball))
    val beaconIntervals = ballsAndBeacons.collect {
      case bb if bb.beacon.y == targetY => Interval(bb.beacon.x, bb.beacon.x)
    }
    intervals.flatMap(interval => Interval.diffAll(interval, beaconIntervals))

  @main
  def solution1(): Unit =
    val minX = ballsAndBeacons.map { ballAndBeacon =>
      ballAndBeacon.ball.centre.x - ballAndBeacon.ball.radius
    }.min
    val maxX = ballsAndBeacons.map { ballAndBeacon =>
      ballAndBeacon.ball.centre.x + ballAndBeacon.ball.radius
    }.max
    val targetY = 2000000
    val yInterval = Interval(minX, maxX)
    val fullSize = Interval.length(yInterval)
    val onTargetY = intersectAllWithY(targetY = targetY)
    pprint.log(onTargetY)
    val diffInterval = Interval.diffAll(yInterval, onTargetY)
    val subSizes = diffInterval.map(Interval.length).sum
    // A <= X ==> |X \ A| = |X| - |A|, i.e. |A| = |X| - |X \ A|
    pprint.log(fullSize - subSizes)

  // Sketchy from here. Compute the same value for the computed targetY, as in part 1.
  //

  def extractSingleValue(interval: Interval): Option[Int] =
    interval match
      case Interval.NonEmpty(lower, upper) if lower == upper => Some(lower)
      case _                                                 => None

  @main
  def solution2(): Unit =
    val minBound = 0
    val maxBound = 4000000
    val bounded = Interval(minBound, maxBound)
    // Find the first (and only) such that there is a non-covered point on the grid.
    // Takes a few seconds (10-20s) to complete.
    val targetY =
      minBound
        .to(maxBound)
        .iterator
        .map { targetY =>
          val s = intersectAllWithY(targetY)
          val beaconIntervals = ballsAndBeacons.map(_.beacon).distinct.collect {
            case beacon if beacon.y == targetY => Interval(beacon.x, beacon.x)
          }
          targetY -> Interval.diffAll(bounded, s ++ beaconIntervals).map(Interval.length).sum
        }
        .collectFirst { case (y, size) if size > 0 => y }
        .get
    pprint.log(targetY)
    val onTargetY = intersectAllWithY(targetY = targetY)
    // By precondition: Only one interval can occur.
    val diffInterval = Interval.diffAll(bounded, onTargetY).head
    // By precondition: Only a single point possible
    val targetX = extractSingleValue(diffInterval).get

    val bigInt = BigInt(targetX) * BigInt(4000000) + BigInt(targetY)
    pprint.log(bigInt)

}
