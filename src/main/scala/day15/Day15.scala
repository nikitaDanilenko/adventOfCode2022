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

//  @main
//  def solution2(): Unit =
//    val minBound = 0
//    val maxBound = 4000000
//    val all = minBound.to(maxBound).toSet
//    val lines = for {
//      targetY <- minBound.to(maxBound)
//    } yield {
//      pprint.log(targetY)
//      targetY -> intersectAllWithY(targetY).filter(p => p.x >= minBound && p.x <= maxBound)
//    }
//    val incomplete =
//      lines.iterator
//        .map { case (y, s) =>
//          all.map(Pos(_, y)).diff(s)
//        }
//        .collectFirst {
//          case line if line.nonEmpty => line
//        }
//    pprint.log(incomplete)

}
