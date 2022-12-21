package day21

import algebra.ring.Semiring
import spire.algebra.EuclideanRing
import spire.math.Rational

sealed trait Poly

object Poly {
  case class Constant(c0: Rational) extends Poly
  case class Linear(c1: Rational, c0: Rational) extends Poly

  // Partial instance, the missing implementations should not be necessary for the task.
  // Simplified assumption, since otherwise one needs to switch to proper polynomials.
  // Surprisingly the spire polynomials are too slow.
  implicit val euclideanRing: EuclideanRing[Poly] = new EuclideanRing[Poly]:
    override def zero: Poly = Constant(Rational(0))

    override def times(x: Poly, y: Poly): Poly = (x, y) match
      case (Constant(c), Constant(d)) => Constant(c * d)
      case (Constant(c), Linear(c1, c0)) => Linear(c * c1, c * c0)
      case (Linear(c1, c0), Constant(c)) => Linear(c * c1, c * c0)
      case _ => ???

    override def euclideanFunction(a: Poly): BigInt = ???

    override def equot(a: Poly, b: Poly): Poly = (a, b) match
      case (Constant(c), Constant(d)) => Constant(c / d)
      case (Linear(c1, c0), Constant(c)) => {
        if (c1 / c == 0)
          pprint.log("Division yields zero")
        Linear(c1 / c, c0 / c)
      }
      case _ => ???

    override def emod(a: Poly, b: Poly): Poly = ???

    override def negate(x: Poly): Poly = x match
      case Constant(c0) => Constant(-c0)
      case Linear(c1, c0) => Linear(-c1, -c0)

    override def one: Poly = Constant(Rational(1))

    override def plus(x: Poly, y: Poly): Poly = (x, y) match
      case (Constant(c), Constant(d)) => Constant(c + d)
      case (Constant(c), Linear(c1, c0)) => Linear(c1, c + c0)
      case (Linear(c1, c0), Constant(c)) => Linear(c1, c + c0)
      case (Linear(c1, c0), Linear(d1, d0)) => {
        if (c1 + d1 == 0)
          pprint.log("cancel out")
        Linear(c1 + d1, c0 + d0)
      }





}
