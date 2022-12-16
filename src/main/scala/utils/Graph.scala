package utils

import algebra.ring.Semiring
import spire.syntax.semiring._

case class Graph[Node](
    adjacency: Map[Node, List[Node]]
)

object Graph {

  def layers[Node](from: Set[Node], graph: Graph[Node]): LazyList[Set[Node]] = {
    def step(current: Set[Node], unseen: Set[Node]): LazyList[Set[Node]] =
      if (current.isEmpty) LazyList.empty
      else {
        val newUnseen = unseen.diff(current)
        val newCurrent = current.flatMap(graph.adjacency.getOrElse(_, List.empty).toSet).intersect(newUnseen)
        current #:: step(newCurrent, newUnseen)
      }

    step(from, unseen = graph.adjacency.keySet)
  }

  case class Distances[Node](
      perNode: Map[Node, Map[Node, Tropical]]
  )

  sealed trait Tropical

  object Tropical {
    case class Value(v: Int) extends Tropical
    case object PositiveInf extends Tropical

    implicit val semiringTropical: Semiring[Tropical] = new Semiring[Tropical] {
      override val zero: Tropical = PositiveInf

      override def times(x: Tropical, y: Tropical): Tropical = (x, y) match
        case (PositiveInf, _)     => PositiveInf
        case (_, PositiveInf)     => PositiveInf
        case (Value(a), Value(b)) => Value(a + b)

      override def plus(x: Tropical, y: Tropical): Tropical = (x, y) match
        case (PositiveInf, _)     => y
        case (_, PositiveInf)     => x
        case (Value(a), Value(b)) => Value(math.min(a, b))

    }

  }

  def shortestPaths[Node](graph: Graph[Node]): Distances[Node] =
    val initial =
      Distances(
        graph.adjacency.view
          .mapValues(_.map(_ -> Tropical.Value(1)).toMap)
          .toMap
      )

    def next(distances: Distances[Node], node: Node): Distances[Node] =
      val ds = distances.perNode.view.mapValues { aj =>
        val scalar = aj.getOrElse(node, Tropical.PositiveInf)
        val line = distances.perNode(node)
        CollectionUtil.unionWith(
          line.view.mapValues(scalar * _).toMap,
          aj
        )(_ + _)
      }.toMap
      Distances(ds)

    graph.adjacency.keys.foldLeft(initial)(next)

}
