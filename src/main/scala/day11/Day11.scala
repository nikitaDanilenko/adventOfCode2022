package day11

import scala.annotation.tailrec
import scala.util.chaining.*

object Day11 {

  type Monkeys = Map[Int, Monkey]

  val testInput: Monkeys = Map(
    0 -> Monkey(
      items = List(79, 98),
      operation = 19 * _,
      divisibleBy = 23,
      onTrue = 2,
      onFalse = 3
    ),
    1 -> Monkey(
      items = List(54, 65, 75, 74),
      operation = 6 + _,
      divisibleBy = 19,
      onTrue = 2,
      onFalse = 0
    ),
    2 -> Monkey(
      items = List(79, 60, 97),
      operation = i => i * i,
      divisibleBy = 13,
      onTrue = 1,
      onFalse = 3
    ),
    3 -> Monkey(
      items = List(74),
      operation = 3 + _,
      divisibleBy = 17,
      onTrue = 0,
      onFalse = 1
    )
  )

  lazy val input: Monkeys = Map(
    0 -> Monkey(
      items = List(91, 66),
      operation = 13 * _,
      divisibleBy = 19,
      onTrue = 6,
      onFalse = 2
    ),
    1 -> Monkey(
      items = List(78, 97, 59),
      operation = 7 + _,
      divisibleBy = 5,
      onTrue = 0,
      onFalse = 3
    ),
    2 -> Monkey(
      items = List(57, 59, 97, 84, 72, 83, 56, 76),
      operation = 6 + _,
      divisibleBy = 11,
      onTrue = 5,
      onFalse = 7
    ),
    3 -> Monkey(
      items = List(81, 78, 70, 58, 84),
      operation = 5 + _,
      divisibleBy = 17,
      onTrue = 6,
      onFalse = 0
    ),
    4 -> Monkey(
      items = List(60),
      operation = 8 + _,
      divisibleBy = 7,
      onTrue = 1,
      onFalse = 3
    ),
    5 -> Monkey(
      items = List(57, 69, 63, 75, 62, 77, 72),
      operation = 5 * _,
      divisibleBy = 13,
      onTrue = 7,
      onFalse = 4
    ),
    6 -> Monkey(
      items = List(73, 66, 86, 79, 98, 87),
      operation = n => n * n,
      divisibleBy = 3,
      onTrue = 5,
      onFalse = 2
    ),
    7 -> Monkey(
      items = List(95, 89, 63, 67),
      operation = 2 + _,
      divisibleBy = 2,
      onTrue = 1,
      onFalse = 4
    )
  )

  def playRound(monkeys: Monkeys, divisionBy: Int, quotient: BigInt): Monkeys = {
    monkeys.keys.toList.sorted.foldLeft(monkeys) { (map, monkeyIndex) =>
      val monkey = map(monkeyIndex)
      val updatedMap = monkey.items.foldLeft(map) { (m, item) =>
        val updatedItem = (monkey.operation.apply(item) % quotient) / divisionBy
        if (updatedItem                                 % monkey.divisibleBy == 0)
          m.updated(monkey.onTrue, Monkey.addItem(m(monkey.onTrue), updatedItem))
        else
          m.updated(monkey.onFalse, Monkey.addItem(m(monkey.onFalse), updatedItem))
      }
      updatedMap.updated(monkeyIndex, Monkey.clearAfterInspectionItems(monkey))
    }
  }

  def play(monkeys: Monkeys, rounds: Int, divisionBy: Int): BigInt = {
    val quotient = monkeys.values.map(_.divisibleBy).product
    List
      .iterate(monkeys, 1 + rounds)(playRound(_, divisionBy, quotient))
      .last
      .toList
      .map(_._2.inspected)
      .sortWith(_ >= _)
      .take(2)
      .product
  }

  @main
  def solution1(): Unit =
    input
      .pipe(play(_, 20, 3))
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    input
      .pipe(play(_, 10000, 1))
      .pipe(pprint.log(_))

}
