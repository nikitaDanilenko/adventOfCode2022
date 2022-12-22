package day22

import utils.Pos

import scala.io.Source

object Day22 {

  type AreaMap = Map[Pos, AreaType]

  case class Input(
      areaMap: AreaMap,
      instructions: List[Instruction]
  )

  val input: Input = {
    val lines = Source.fromResource("real/day22.txt").getLines()
    val (map, is) = lines.span(_.nonEmpty)
    val instructions = Instruction.parser.rep.parse(is.toList.drop(1).head).toOption.get._2.toList
    val areaMap = map
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (line, j) =>
        line.flatMap((c, i) => AreaType.parser.parse(s"$c").toOption.map(_._2).map(Pos(i, j) -> _))
      }
      .toMap

    Input(areaMap, instructions)
  }

  def move(
      areaMap: AreaMap,
      location: Location,
      instruction: Instruction
  ): Location = {
    lazy val xs = areaMap.collect { case (pos, _) if pos.y == location.pos.y => pos.x }
    lazy val ys = areaMap.collect { case (pos, _) if pos.x == location.pos.x => pos.y }
    instruction match
      case Instruction.Move(units) =>
        location.facingDirection match
          case FacingDirection.L =>
            val newPosition = 1.to(units).foldLeft(location.pos) { (pos, _) =>
              val xCandidate = if pos.x - 1 >= xs.min then pos.x - 1 else xs.max
              val newPos = pos.copy(x = xCandidate)
              areaMap(newPos) match
                case AreaType.Free => newPos
                case AreaType.Wall => pos
            }
            location.copy(pos = newPosition)
          case FacingDirection.R =>
            val newPosition = 1.to(units).foldLeft(location.pos) { (pos, _) =>
              val xCandidate = if pos.x + 1 <= xs.max then pos.x + 1 else xs.min
              val newPos = pos.copy(x = xCandidate)
              areaMap(newPos) match
                case AreaType.Free => newPos
                case AreaType.Wall => pos
            }
            location.copy(pos = newPosition)
          case FacingDirection.U =>
            val newPosition = 1.to(units).foldLeft(location.pos) { (pos, _) =>
              val yCandidate = if pos.y - 1 >= ys.min then pos.y - 1 else ys.max
              val newPos = pos.copy(y = yCandidate)
              areaMap(newPos) match
                case AreaType.Free => newPos
                case AreaType.Wall => pos
            }
            location.copy(pos = newPosition)
          case FacingDirection.D =>
            val newPosition = 1.to(units).foldLeft(location.pos) { (pos, _) =>
              val yCandidate = if pos.y + 1 <= ys.max then pos.y + 1 else ys.min
              val newPos = pos.copy(y = yCandidate)
              areaMap(newPos) match
                case AreaType.Free => newPos
                case AreaType.Wall => pos
            }
            location.copy(pos = newPosition)
      case Instruction.Rotate(rotatingDirection) =>
        location.copy(
          facingDirection = FacingDirection.rotate(location.facingDirection, rotatingDirection)
        )
  }

  def iteratedMove(areaMap: AreaMap, start: Location, instructions: List[Instruction]): Location =
    instructions.foldLeft(start)(move(areaMap, _, _))

  def startOf(areaMap: AreaMap): Location =
    val minY = areaMap.keys.map(_.y).min
    val minX = areaMap.collect { case (pos, _) if pos.y == minY => pos.x }.min
    Location(
      Pos(minX, minY),
      FacingDirection.R
    )

  def password(location: Location): Int =
    // offset positions, only relevant here
    1000 * (location.pos.y + 1) + 4 * (location.pos.x + 1) + FacingDirection.passwordValue(location.facingDirection)

  @main
  def solution1(): Unit =
    val start = startOf(input.areaMap)
    val finalLocation = iteratedMove(input.areaMap, start, input.instructions)
    val result = password(finalLocation)
    pprint.log(result)

}
