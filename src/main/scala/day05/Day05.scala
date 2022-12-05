package day05

import scala.io.Source
import scala.util.chaining._

object Day05 {

  val commands: Iterator[Command] = Source
    .fromResource("day05.txt")
    .getLines()
    .flatMap(
      Command.parser
        .parse(_)
        .toOption
        .map(_._2)
    )

  //First item is the top-most one
  type Stacks = Map[Int, Vector[Char]]

  def applyCommand(stacks: Stacks, command: Command): Stacks = {
    val at = stacks(command.from)
    val (removed, remaining) = at.splitAt(command.amount)
    stacks
      .updated(command.from, remaining)
      .updated(command.to, removed.reverse ++ stacks(command.to))
  }

  val inputStacks: Stacks = Map(
    1 -> "JFCNDBW",
    2 -> "TSLQVZP",
    3 -> "TJGBZP",
    4 -> "CHBZJLTD",
    5 -> "SJBVG",
    6 -> "QSP",
    7 -> "NPMLFDVB",
    8 -> "RLDBFMSP",
    9 -> "RTDV"
  ).view
    .mapValues(_.toVector)
    .toMap

  val testStacks: Stacks = Map(
    1 -> "NZ",
    2 -> "DCM",
    3 -> "P"
  ).view
    .mapValues(_.toVector)
    .toMap

  @main
  def solution1(): Unit =
    commands
      .foldLeft(inputStacks)(applyCommand)
      .view
      .mapValues(_.head)
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString
      .pipe(pprint.log(_))

}
