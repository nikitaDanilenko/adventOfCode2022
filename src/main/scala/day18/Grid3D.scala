package day18

import cats.parse.Parser
import utils.ParserUtil

case class Grid3D(
    x: Int,
    y: Int,
    z: Int
)

object Grid3D {

  val parser: Parser[Grid3D] = for {
    x <- ParserUtil.int
    _ <- Parser.char(',')
    y <- ParserUtil.int
    _ <- Parser.char(',')
    z <- ParserUtil.int
  } yield Grid3D(x, y, z)

  def neighbours(grid3D: Grid3D): Set[Grid3D] =
    Set(
      grid3D.copy(x = grid3D.x - 1),
      grid3D.copy(x = grid3D.x + 1),
      grid3D.copy(y = grid3D.y - 1),
      grid3D.copy(y = grid3D.y + 1),
      grid3D.copy(z = grid3D.z - 1),
      grid3D.copy(z = grid3D.z + 1)
    )
    
  def connected(grid3D1: Grid3D, grid3D2: Grid3D): Boolean =
    neighbours(grid3D1).contains(grid3D2)

}
