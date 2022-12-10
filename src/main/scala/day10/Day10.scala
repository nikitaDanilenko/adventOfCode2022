package day10

import scala.io.Source
import scala.util.chaining._

object Day10 {

  val input: List[Command] = Source
    .fromResource("real/day10.txt")
    .getLines()
    .flatMap(Command.parser.parse(_).toOption.map(_._2))
    .toList

  def processCommands(commands: List[Command]): List[Int] =
    val cycleInstructions = commands.flatMap(Command.toCycleInstructions)
    cycleInstructions.scanLeft(1) { (register, command) =>
      command match
        case CycleInstruction.Wait            => register
        case CycleInstruction.AddValue(value) => register + value
    }

  def visualize(commands: List[Command]): List[List[Char]] =
    def spriteArea(value: Int): Set[Int] = Set(value - 1, value, value + 1)
    processCommands(commands)
      .grouped(40)
      .toList
      .map(
        _.zipWithIndex
          .map { case (value, index) =>
            if (spriteArea(value).contains(index)) '#' else '.'
          }
      )

  @main
  def solution1(): Unit =
    input
      .pipe(processCommands)
      .pipe(_.zipWithIndex)
      .pipe(_.map { case (value, index) => (value, index + 1) })
      .pipe(_.collect { case (value, index) if List(20, 60, 100, 140, 180, 220).contains(index) => value * index })
      .pipe(_.sum)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    input
      .pipe(visualize)
      .pipe(_.map(_.mkString).mkString("\n"))
      .pipe(println)

}
