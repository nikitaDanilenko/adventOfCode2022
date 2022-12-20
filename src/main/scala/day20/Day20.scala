package day20

import scala.io.Source

object Day20 {

  val input: List[Int] = Source
    .fromResource("real/day20.txt")
    .getLines()
    .map(_.toInt)
    .toList

  def insertAtIndex[A](index: Int, value: A, list: List[A]): List[A] =
    val (beforeIndex, afterEqIndex) = list.splitAt(index)
    beforeIndex ++ (value +: afterEqIndex)

  def wrapIndex(positionalIndex: Int, listSize: Int, value: Long): Int =
    val mod = listSize - 1
    val movedPositional = ((positionalIndex + value) % mod).intValue()
    if movedPositional < 0 then movedPositional + mod
    else if movedPositional >= listSize then movedPositional - mod
    else movedPositional

  def moveValue(value: Long, index: Int, list: List[(Long, Int)], listSize: Int): List[(Long, Int)] =
    val positionalIndex = list.indexWhere(_._2 == index)
    val listWithoutIndex = list.filter(p => p._2 != index)
    val newIndex = wrapIndex(positionalIndex, listSize, value)
    insertAtIndex(newIndex, (value, index), listWithoutIndex)

  def iteratedMove(initial: List[Long], listSize: Int, repetitions: Int): List[Long] =
    val indexedInitial = initial.zipWithIndex
    1.to(repetitions)
      .foldLeft(indexedInitial) { (mixed, _) =>
        indexedInitial
          .foldLeft(mixed) { case (moved, (value, index)) =>
            moveValue(value, index, moved, listSize)
          }
      }
      .map(_._1)

  def findCoordinates(list: List[Long], listSize: Int): Long =
    val zeroIndex = list.indexOf(0)
    List(1000, 2000, 3000).map { offset =>
      list((offset + zeroIndex) % listSize)
    }.sum

  @main
  def solution1(): Unit =
    val initial = input.map(_.toLong)
    val size = initial.size
    pprint.log(findCoordinates(iteratedMove(initial, size, 1), size))

  @main
  def solution2(): Unit =
    val initial = input.map(_ * 811589153L)
    val size = initial.size
    pprint.log(findCoordinates(iteratedMove(initial, size, 10), size))

}
