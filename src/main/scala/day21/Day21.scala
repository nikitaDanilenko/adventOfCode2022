package day21

import algebra.ring.Semiring
import cats.data.State
import spire.algebra.EuclideanRing
import spire.implicits.*

import scala.io.Source

object Day21 {

  val input: List[Monkey] =
    Source
      .fromResource("real/day21.txt")
      .getLines()
      .flatMap(Monkey.parser.parse(_).toOption.map(_._2))
      .toList

  def evaluate[R: Semiring: EuclideanRing: EmbeddableIntegers](list: List[Monkey]): R =
    val monkeyMap = list.map(m => m.name -> m).toMap
    type MonkeyValues = Map[String, R]

    def iterate(monkey: Monkey): State[MonkeyValues, R] =
      for {
        stateMap <- State.get[MonkeyValues]
        result <- stateMap
          .get(monkey.name)
          .fold {
            val Monkey.Computation(name, op1, op2, operation) = monkey
            for {
              x1 <- iterate(monkeyMap(op1))
              x2 <- iterate(monkeyMap(op2))
              result = Operation[R](operation).apply(x1, x2)
              _ <- State.modify[MonkeyValues](_.updated(name, result))
            } yield result
          }(State.pure)
      } yield result

    val initial = list.collect { case Monkey.Number(name, value) => name -> EmbeddableIntegers[R].embed(value) }.toMap
    val root = list.collectFirst { case m if m.name == "root" => m }.get
    iterate(root).run(initial).value._2

  @main
  def solution1(): Unit =
    val result = evaluate[BigInt](input)
    pprint.log(result)

}
