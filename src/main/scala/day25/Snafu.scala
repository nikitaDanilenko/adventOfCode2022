package day25

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import spire.math.Natural
import util.chaining._

case class Snafu(
    digits: NonEmptyList[Snafu.Digit]
)

object Snafu {

  enum Digit:
    case MinusTwo, MinusOne, Zero, One, Two

  object Digit {

    def toString(digit: Digit): String = digit match
      case Digit.MinusTwo => "="
      case Digit.MinusOne => "-"
      case Digit.Zero     => "0"
      case Digit.One      => "1"
      case Digit.Two      => "2"

    def fromString(string: String): Option[Digit] = Some(string).collect {
      case "=" => Digit.MinusTwo
      case "-" => Digit.MinusOne
      case "0" => Digit.Zero
      case "1" => Digit.One
      case "2" => Digit.Two
    }

    def toNumber(digit: Digit): BigInt = digit match
      case Digit.MinusTwo => -2
      case Digit.MinusOne => -1
      case Digit.Zero     => 0
      case Digit.One      => 1
      case Digit.Two      => 2

  }

  def toNumber(snafu: Snafu): BigInt =
    snafu.digits.reverse.zipWithIndex.foldLeft(BigInt(0)) { case (sum, (d, pos)) =>
      sum + spire.math.pow(BigInt(5), BigInt(pos)) * Digit.toNumber(d)
    }

  def fromString(string: String): Option[Snafu] =
    string.toList
      .traverse(c => Digit.fromString(s"$c"))
      .flatMap(NonEmptyList.fromList)
      .map(Snafu.apply)

  def fromNumber(natural: Natural): Snafu =
    def digits(n: BigInt): List[Digit] =
      if n <= 0 then List.empty
      else
        val div = n / 5
        val mod = n % 5

        val (digit, next) = mod match {
          case 0 => (Digit.Zero, div)
          case 1 => (Digit.One, div)
          case 2 => (Digit.Two, div)
          case 3 => (Digit.MinusTwo, 1 + div)
          case 4 => (Digit.MinusOne, 1 + div)
        }

        digit :: digits(next)
    natural.toBigInt
      .pipe(digits)
      .pipe(_.reverse)
      .pipe(NonEmptyList.fromList)
      .pipe(_.getOrElse(NonEmptyList.of(Digit.Zero)))
      .pipe(Snafu)

  def toString(snafu: Snafu): String =
    snafu.digits.toList.map(Digit.toString).mkString

}

object Main {

  @main
  def run: Unit =
    val snafu = Snafu.fromString("2=-01").get
    pprint.log(snafu)
    pprint.log(Snafu.toNumber(snafu))
    pprint.log(Snafu.fromNumber(Natural(976)).pipe(Snafu.toString))

}
