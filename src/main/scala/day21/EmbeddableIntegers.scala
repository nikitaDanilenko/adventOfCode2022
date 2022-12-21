package day21

import spire.math.Polynomial
import spire.implicits._

@FunctionalInterface
trait EmbeddableIntegers[R] {
  def embed(bigInt: BigInt): R
}

object EmbeddableIntegers {
  def apply[R](implicit embeddableIntegers: EmbeddableIntegers[R]): EmbeddableIntegers[R] = embeddableIntegers

  implicit val embeddableBigInt: EmbeddableIntegers[BigInt] = identity
  implicit val embeddablePolynomials: EmbeddableIntegers[Polynomial[BigInt]] = Polynomial.constant[BigInt]
}
