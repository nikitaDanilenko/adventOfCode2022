package day18

import utils.{ CollectionUtil, Graph }

import scala.io.Source
import scala.util.chaining.*

object Day18 {

  val input: List[Grid3D] =
    Source
      .fromResource("real/day18.txt")
      .getLines()
      .flatMap(Grid3D.parser.parse(_).toOption.map(_._2))
      .toList

  def freeFaces(points: List[Grid3D]): List[Grid3D] =
    val pointSet = points.toSet
    points
      .flatMap(Grid3D.neighbours(_).toList)
      .filter(!pointSet.contains(_))

  def minToMax(xs: Iterable[Int]): Iterable[Int] =
    if xs.isEmpty then Iterable.empty[Int] else xs.min.to(xs.max)

  def minToMaxOverlap(xs: Iterable[Int]): Iterable[Int] =
    if xs.isEmpty then Iterable.empty[Int] else (xs.min - 1).to(xs.max + 1)

  def reachableFromOutside(points: List[Grid3D]): Set[Grid3D] =
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    val zs = points.map(_.z)
    val pointSet = points.toSet
    val initial = Grid3D(
      xs.min - 1,
      ys.min - 1,
      zs.min - 1
    )
    // Choose slightly larger cuboid that contains all blocks
    val cuboid = for {
      x <- minToMaxOverlap(xs)
      y <- minToMaxOverlap(ys)
      z <- minToMaxOverlap(zs)
    } yield Grid3D(x, y, z)

    val graph = cuboid
      .map { g =>
        g -> Grid3D.neighbours(g).diff(pointSet).toList
      }
      .toMap
      .pipe(Graph(_))
    // Any point in the extra space is a valid starting point.
    // Perform a BFS from there to find all those grid elements that can be reached from outside.
    Graph.layers(from = Set(initial), graph).foldLeft(Set.empty[Grid3D])(_.union(_))

  def unreachableInner(points: List[Grid3D]): Set[Grid3D] =
    val reachable = reachableFromOutside(points)
    val pointSet = points.toSet
    val zs = points.map(_.z)
    // Traverse layer-wise
    val missing = for {
      z <- zs.min.to(zs.max)
      zPlane = points.filter(_.z == z)
      ys = zPlane.map(_.y)
      y <- minToMax(ys)
    } yield {
      // For each line find the difference between the whole line,
      // and those elements that are missing (i.e. not contained in the point set),
      // and not reachable from outside.
      val xs = zPlane.collect { case p if p.y == y => p.x }
      minToMax(xs).map(Grid3D(_, y, z)).filter(p => !pointSet.contains(p) && !reachable.contains(p))
    }
    missing.flatten.toSet

  @main
  def solution1(): Unit =
    val faces = freeFaces(input)
    pprint.log(faces.size)

  @main
  def solution2(): Unit =
    val unreachable = unreachableInner(input)
    val total = freeFaces(input).size - freeFaces(unreachable.toList).size
    pprint.log(total)

}
