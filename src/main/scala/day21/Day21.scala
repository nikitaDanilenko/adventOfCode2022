package day21

import cats.data.State

import scala.io.Source

object Day21 {

  val input: List[Monkey] =
    Source
      .fromResource("real/day21.txt")
      .getLines()
      .flatMap(Monkey.parser.parse(_).toOption.map(_._2))
      .toList

  def evaluate(list: List[Monkey]): BigInt =
    val monkeyMap = list.map(m => m.name -> m).toMap
    type MonkeyValues = Map[String, BigInt]

    def iterate(monkey: Monkey): State[MonkeyValues, BigInt] =
      for {
        stateMap <- State.get[MonkeyValues]
        result <- stateMap
          .get(monkey.name)
          .fold {
            val Monkey.Computation(name, op1, op2, operation) = monkey
            for {
              x1 <- iterate(monkeyMap(op1))
              x2 <- iterate(monkeyMap(op2))
              result = Operation(operation)(x1, x2)
              _ <- State.modify[MonkeyValues](_.updated(name, result))
            } yield result
          }(State.pure)
      } yield result

    val initial = list.collect { case Monkey.Number(name, value) => name -> value }.toMap
    val root = list.collectFirst { case m if m.name == "root" => m }.get
    iterate(root).run(initial).value._2

  @main
  def solution1(): Unit =
    val result = evaluate(input)
    pprint.log(result)

}
