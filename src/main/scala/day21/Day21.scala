package day21

import algebra.ring.Semiring
import cats.data.State
import spire.algebra.EuclideanRing
import spire.implicits.*
import spire.math.{Polynomial, Rational}

import scala.io.Source

object Day21 {

  val input: List[Monkey] =
    Source
      .fromResource("real/day21.txt")
      .getLines()
      .flatMap(Monkey.parser.parse(_).toOption.map(_._2))
      .toList

  type MonkeyValues[R] = Map[String, R]

  def evaluate[R: Semiring: EuclideanRing](
      list: List[Monkey],
      start: Monkey
  ): State[MonkeyValues[R], R] =
    val monkeyMap = list.map(m => m.name -> m).toMap

    def iterate(monkey: Monkey): State[MonkeyValues[R], R] =
      for {
        stateMap <- State.get[MonkeyValues[R]]
        result <- stateMap
          .get(monkey.name)
          .fold {
            val Monkey.Computation(name, op1, op2, operation) = monkey
            for {
              x1 <- iterate(monkeyMap(op1))
              x2 <- iterate(monkeyMap(op2))
              result = Operation[R](operation).apply(x1, x2)
              _ <- State.modify[MonkeyValues[R]](_.updated(name, result))
            } yield result
          }(State.pure)
      } yield result

    iterate(start)

  def findMonkeyByName(name: String): Monkey =
    input.find(_.name == name).get

  @main
  def solution1(): Unit =
    val root = findMonkeyByName("root")
    val resultState = evaluate[BigInt](input, root)
    val initial = input.collect { case Monkey.Number(name, value) => name -> value }.toMap
    val result = resultState.run(initial).value._2
    pprint.log(result)

  @main
  def solution2(): Unit =
    val Monkey.Computation(_, x1, x2, _) = findMonkeyByName("root")
    val m1 = findMonkeyByName(x1)
    val m2 = findMonkeyByName(x2)
    val initial = input.collect { case Monkey.Number(name, value) =>
      val polynomial =
        if name == "humn" then Polynomial.linear(Rational(1)) else Polynomial.constant(Rational(value))
      name -> polynomial
    }.toMap
    val p1 = evaluate[Polynomial[Rational]](input, m1).run(initial).value._2
    val p2 = evaluate[Polynomial[Rational]](input, m2).run(initial).value._2

    val (equation, constant) = if p1.maxTerm.exp > p2.maxTerm.exp then (p1, p2) else (p2, p1)
    val result = (constant.coeffsArray.apply(0) - equation.coeffsArray.apply(0)) / equation.coeffsArray.apply(1)
    pprint.log(result)


}
