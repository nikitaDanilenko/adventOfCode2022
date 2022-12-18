package day18

import utils.CollectionUtil

import scala.io.Source

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

  @main
  def solution1(): Unit =
    val faces = freeFaces(input)
    pprint.log(faces.size)

}
