package day12

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

}
