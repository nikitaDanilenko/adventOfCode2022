package day02

import cats.parse.Parser
import day02.Day02.RPS.Rock

import scala.io.Source

object Day02 {

  enum RPS:
    case Rock, Paper, Scissors

  def parserFromEncoding(rock: Char, paper: Char, scissors: Char): Parser[RPS] =
    Parser.oneOf(
      List(
        Parser.char(rock).map(_ => RPS.Rock),
        Parser.char(paper).map(_ => RPS.Paper),
        Parser.char(scissors).map(_ => RPS.Scissors)
      )
    )

  val rpsParserOpponent: Parser[RPS] =
    parserFromEncoding('A', 'B', 'C')

  val rpsParserOwn: Parser[RPS] =
    parserFromEncoding('X', 'Y', 'Z')

  case class Round(
      opponent: RPS,
      own: RPS
  )

  val roundParser: Parser[Round] =
    for {
      opponent <- rpsParserOpponent
      _ <- Parser.char(' ')
      own <- rpsParserOwn
    } yield Round(
      opponent = opponent,
      own = own
    )

  enum Outcome:
    case Loss, Draw, Win

  def ownOutcome(round: Round): Outcome =
    (round.opponent, round.own) match {
      case (x, y) if x == y                                                             => Outcome.Draw
      case (RPS.Rock, RPS.Scissors) | (RPS.Scissors, RPS.Paper) | (RPS.Paper, RPS.Rock) => Outcome.Loss
      case _                                                                            => Outcome.Win
    }

  def outcomePoints(outcome: Outcome): Int = outcome match
    case Outcome.Loss => 0
    case Outcome.Draw => 3
    case Outcome.Win  => 6

  def points(rps: RPS): Int =
    rps match
      case RPS.Rock     => 1
      case RPS.Paper    => 2
      case RPS.Scissors => 3

  def score(round: Round): Int =
    outcomePoints(ownOutcome(round)) + points(round.own)

  lazy val input: Iterator[Round] = Source
    .fromResource("day02.txt")
    .getLines()
    .flatMap(roundParser.parse(_).map(_._2).toOption)

  @main
  def solution1(): Unit =
    val result = input
      .map(score)
      .sum
    pprint.log(result)

}
