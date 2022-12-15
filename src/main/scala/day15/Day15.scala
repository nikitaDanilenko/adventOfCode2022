package day15

import cats.Order
import spire.math.interval.{ Bound, Closed, EmptyBound, Open, Unbound, ValueBound }
import spire.math.{ Interval, min }
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

  def diffAll[A: Order](interval: Interval[A], diffs: List[Interval[A]]): List[Interval[A]] =
    diffs.foldLeft(List(interval))((is, d) => is.flatMap(_ -- d))

  def intersectAllWithY(targetY: Int): List[Interval[Int]] =
    val intervals = ballsAndBeacons
      .map(ballAndBeacon => P1Ball.intersectAtY(targetY, ballAndBeacon.ball))
    val beaconIntervals = ballsAndBeacons.map(bb => Interval.point(bb.beacon.x)) // overestimate
    intervals.flatMap(interval => diffAll(interval, beaconIntervals))

  def extractValue[A](bound: Bound[A]): Option[A] = bound match
    case EmptyBound() => None
    case Unbound()    => None
    case Open(a)      => Some(a)
    case Closed(a)    => Some(a)

  def length(interval: Interval[Int]): Int =
    val l = for {
      lower <- extractValue(interval.lowerBound)
      upper <- extractValue(interval.upperBound)
    } yield upper - lower
    l.getOrElse(0)

  @main
  def solution1(): Unit =
    val minX = ballsAndBeacons.map { ballAndBeacon =>
      ballAndBeacon.ball.centre.x - ballAndBeacon.ball.radius
    }.min
    val maxX = ballsAndBeacons.map { ballAndBeacon =>
      ballAndBeacon.ball.centre.x + ballAndBeacon.ball.radius
    }.max
    val targetY = 2000000
    val yInterval = Interval.closed(minX, maxX)
    val fullSize = length(yInterval)
    val onTargetY = intersectAllWithY(targetY = targetY)
    val diffInterval = diffAll(yInterval, onTargetY)
    val subSizes = diffInterval.map(length).sum
    // A <= X ==> |X \ A| = |X| - |A|, i.e. |A| = |X| - |X \ A|
    pprint.log(fullSize - subSizes)

  // Sketchy from here. Compute the same value for the computed targetY, as in part 1.
  //

  @main
  def preSolution2(): Unit =
    val minBound = 0
    val maxBound = 4000000
    val bounded = Interval.closed(minBound, maxBound)
    // Find the first (and only) such that there is a non-covered point on the grid
    val targetY =
      minBound
        .to(maxBound)
        .iterator
        .map { targetY =>
          val s = intersectAllWithY(targetY).map(_.intersect(bounded))
          targetY -> diffAll(bounded, s).map(length).sum
        }
        .filter { case (y, size) =>
          if (y % 100000 == 0)
            pprint.log(y)
          size > 0
        }
        .collectFirst { case x => x._1 }
        .get
    pprint.log(targetY)

  // Having completed the preparation, figure out the one missing x for the corresponding y.
  // The result difference contains various singleton intervals and precisely one
  // open interval. The necessary value is found in that open interval.
  // It seems that there is a bug in my implementation, since I would have expected
  // precisely one interval at all.
  //
  // Overall, the second part seems to show that the first part is not as concise as it should be.
  @main
  def finishSolution2(): Unit =
    val minBound = 0
    val maxBound = 4000000
    val bounded = Interval.closed(minBound, maxBound)
    val targetY = 3230812 // computed in the preparation
    val onTargetY = intersectAllWithY(targetY = targetY)
    val diffInterval = diffAll(bounded, onTargetY)
    pprint.log(diffInterval.toString())

    val bigInt = BigInt(3293021) * BigInt(4000000) + BigInt(targetY)
    pprint.log(bigInt)

}
