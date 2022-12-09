package day02

import cats.parse.Parser
import day02.Day02.RPS.Rock

import scala.io.Source
import scala.util.chaining._

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

  case class DesiredOutcome(
      opponent: RPS,
      outcome: Outcome
  )

  val outcomeParser: Parser[Outcome] = Parser.oneOf(
    List(
      Parser.char('X').map(_ => Outcome.Loss),
      Parser.char('Y').map(_ => Outcome.Draw),
      Parser.char('Z').map(_ => Outcome.Win)
    )
  )

  val desiredOutcomeParser = for {
    opponent <- rpsParserOpponent
    _ <- Parser.char(' ')
    outcome <- outcomeParser
  } yield DesiredOutcome(opponent, outcome)

  def complementFor(desiredOutcome: DesiredOutcome): RPS =
    desiredOutcome.outcome match
      case Outcome.Loss =>
        desiredOutcome.opponent match
          case RPS.Rock     => RPS.Scissors
          case RPS.Paper    => RPS.Rock
          case RPS.Scissors => RPS.Paper

      case Outcome.Draw => desiredOutcome.opponent
      case Outcome.Win =>
        desiredOutcome.opponent match
          case RPS.Rock     => RPS.Paper
          case RPS.Paper    => RPS.Scissors
          case RPS.Scissors => RPS.Rock

  lazy val input1: Iterator[Round] = Source
    .fromResource("real/day02.txt")
    .getLines()
    .flatMap(roundParser.parse(_).map(_._2).toOption)

  lazy val input2 = Source
    .fromResource("real/day02.txt")
    .getLines()
    .flatMap(desiredOutcomeParser.parse(_).map(_._2).toOption)

  @main
  def solution1(): Unit =
    val result = input1
      .map(score)
      .sum
    pprint.log(result)

  @main
  def solution2(): Unit =
    val result = input2
      .map(desiredOutcome => Round(desiredOutcome.opponent, complementFor(desiredOutcome)).pipe(score))
      .sum
    pprint.log(result)

}
