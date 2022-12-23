package day22

import utils.Pos

import scala.io.Source

object Day22 {

  type AreaMap = Map[Pos, AreaType]

  case class Input(
      areaMap: AreaMap,
      instructions: List[Instruction]
  )

  val inputLocation: String = "test/day22.txt"

  val input: Input = {
    val lines = Source.fromResource(inputLocation).getLines()
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

  def goOrStop(areaMap: AreaMap, pos: Pos, newPosition: Pos): Pos =
    areaMap(newPosition) match
      case AreaType.Free => newPosition
      case AreaType.Wall => pos

  def move(
      areaMap: AreaMap,
      location: Location,
      instruction: Instruction
  ): Location = {
    lazy val xs = areaMap.collect { case (pos, _) if pos.y == location.pos.y => pos.x }
    lazy val ys = areaMap.collect { case (pos, _) if pos.x == location.pos.x => pos.y }
    instruction match
      case Instruction.Move(units) =>
        val newPosition = location.facingDirection match
          case FacingDirection.L =>
            1.to(units).foldLeft(location.pos) { (pos, _) =>
              val xCandidate = if pos.x - 1 >= xs.min then pos.x - 1 else xs.max
              val newPos = pos.copy(x = xCandidate)
              goOrStop(areaMap, pos, newPos)
            }
          case FacingDirection.R =>
            1.to(units).foldLeft(location.pos) { (pos, _) =>
              val xCandidate = if pos.x + 1 <= xs.max then pos.x + 1 else xs.min
              val newPos = pos.copy(x = xCandidate)
              goOrStop(areaMap, pos, newPos)
            }
          case FacingDirection.U =>
            1.to(units).foldLeft(location.pos) { (pos, _) =>
              val yCandidate = if pos.y - 1 >= ys.min then pos.y - 1 else ys.max
              val newPos = pos.copy(y = yCandidate)
              goOrStop(areaMap, pos, newPos)
            }
          case FacingDirection.D =>
            1.to(units).foldLeft(location.pos) { (pos, _) =>
              val yCandidate = if pos.y + 1 <= ys.max then pos.y + 1 else ys.min
              val newPos = pos.copy(y = yCandidate)
              goOrStop(areaMap, pos, newPos)
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

  type CubeMap = Map[CubeSide, AreaMap]

  def cubeInput(
      sideSize: Int,
      order: List[List[CubeSide]]
  ): CubeMap = {
    Source
      .fromResource(inputLocation)
      .getLines()
      .takeWhile(_.nonEmpty)
      .grouped(sideSize)
      .toList
      .map(
        _.map(
          _.grouped(sideSize)
            .filter(_.trim.nonEmpty)
            .toList
        ).toList.transpose
          .map(_.map(_.zipWithIndex).zipWithIndex.flatMap { case (line, j) =>
            line.flatMap((c, i) => AreaType.parser.parse(s"$c").toOption.map(_._2).map(Pos(i, j) -> _))
          }.toMap)
      )
      .zip(order)
      .flatMap { (maps, sides) =>
        maps.zip(sides).map { case (map, side) => side -> map }
      }
      .toMap
  }

  // Argument: Side, and the direction in which the side is left.
  // Result: Neighbouring side and the direction where the side is entered.
  type Connections = Map[(CubeSide, FacingDirection), (CubeSide, FacingDirection)]

  def goOrStopCube(cubeMap: CubeMap, cubeLocation: CubeLocation, newCubeLocation: CubeLocation): CubeLocation =
    cubeMap(newCubeLocation.cubeSide)(newCubeLocation.location.pos) match
      case AreaType.Free => newCubeLocation
      case AreaType.Wall => cubeLocation

  def stepCube(
      cubeMap: CubeMap,
      cubeLocation: CubeLocation,
      maxSize: Int,
      connections: Connections
  ): CubeLocation =
    lazy val (neighbourSide, direction) = connections(cubeLocation.cubeSide, cubeLocation.location.facingDirection)
    val newLocation =
      cubeLocation.location.facingDirection match
        case FacingDirection.L =>
          if cubeLocation.location.pos.x - 1 >= 0 then
            cubeLocation.copy(
              location = cubeLocation.location.copy(
                pos = cubeLocation.location.pos.copy(x = cubeLocation.location.pos.x - 1)
              )
            )
          else {

            val newPos = direction match
              case FacingDirection.L =>
                Pos(
                  x = maxSize,
                  y = cubeLocation.location.pos.y
                )
              case FacingDirection.R =>
                Pos(
                  x = 0,
                  y = maxSize - cubeLocation.location.pos.y
                )
              case FacingDirection.U =>
                Pos(
                  x = cubeLocation.location.pos.y,
                  y = maxSize
                )
              case FacingDirection.D =>
                Pos(
                  x = cubeLocation.location.pos.y,
                  y = 0
                )
            CubeLocation(
              neighbourSide,
              location = Location(
                newPos,
                direction
              )
            )
          }
        case FacingDirection.R =>
          if cubeLocation.location.pos.x + 1 <= maxSize then
            cubeLocation.copy(
              location = cubeLocation.location.copy(
                pos = cubeLocation.location.pos.copy(x = cubeLocation.location.pos.x + 1)
              )
            )
          else {
            val newPos = direction match
              case FacingDirection.L =>
                Pos(
                  x = maxSize,
                  y = maxSize - cubeLocation.location.pos.y
                )
              case FacingDirection.R =>
                Pos(
                  x = 0,
                  y = cubeLocation.location.pos.y
                )
              case FacingDirection.U =>
                Pos(
                  x = cubeLocation.location.pos.y,
                  y = maxSize
                )
              case FacingDirection.D =>
                Pos(
                  x = maxSize - cubeLocation.location.pos.y,
                  y = 0
                )
            CubeLocation(
              neighbourSide,
              location = Location(
                newPos,
                direction
              )
            )
          }
        case FacingDirection.U =>
          if cubeLocation.location.pos.y - 1 >= 0 then
            cubeLocation.copy(
              location = cubeLocation.location.copy(
                pos = cubeLocation.location.pos.copy(y = cubeLocation.location.pos.y - 1)
              )
            )
          else {
            val newPos = direction match
              case FacingDirection.L =>
                Pos(
                  x = maxSize,
                  y = maxSize - cubeLocation.location.pos.x
                )
              case FacingDirection.R =>
                Pos(
                  x = 0,
                  y = cubeLocation.location.pos.x
                )
              case FacingDirection.U =>
                Pos(
                  x = cubeLocation.location.pos.x,
                  y = maxSize
                )
              case FacingDirection.D =>
                Pos(
                  x = maxSize - cubeLocation.location.pos.x,
                  y = 0
                )
            CubeLocation(
              neighbourSide,
              location = Location(
                newPos,
                direction
              )
            )
          }
        case FacingDirection.D =>
          if cubeLocation.location.pos.y + 1 <= maxSize then
            cubeLocation.copy(
              location = cubeLocation.location.copy(
                pos = cubeLocation.location.pos.copy(y = cubeLocation.location.pos.y + 1)
              )
            )
          else {
            val newPos = direction match
              case FacingDirection.L =>
                Pos(
                  x = maxSize,
                  y = cubeLocation.location.pos.x
                )
              case FacingDirection.R =>
                Pos(
                  x = 0,
                  y = maxSize - cubeLocation.location.pos.x
                )
              case FacingDirection.U =>
                Pos(
                  x = maxSize - cubeLocation.location.pos.x,
                  y = maxSize
                )
              case FacingDirection.D =>
                Pos(
                  x = cubeLocation.location.pos.x,
                  y = 0
                )
            CubeLocation(
              neighbourSide,
              location = Location(
                newPos,
                direction
              )
            )
          }
    goOrStopCube(cubeMap, cubeLocation, newLocation)

  def moveCube(
      cubeMap: CubeMap,
      cubeLocation: CubeLocation,
      instruction: Instruction,
      cubeSize: Int,
      connections: Connections
  ): CubeLocation = {
    instruction match
      case Instruction.Move(units) =>
        1.to(units).foldLeft(cubeLocation) { (currentCubeLocation, _) =>
          stepCube(cubeMap, currentCubeLocation, cubeSize - 1, connections)
        }

      case Instruction.Rotate(rotatingDirection) =>
        cubeLocation.copy(
          location = cubeLocation.location.copy(
            facingDirection = FacingDirection.rotate(cubeLocation.location.facingDirection, rotatingDirection)
          )
        )
  }

  def iteratedMoveCube(
      cubeMap: CubeMap,
      start: CubeLocation,
      instructions: List[Instruction],
      connections: Connections
  ): CubeLocation =
    val size = cubeMap(CubeSide.S0).keySet.map(_.x).size
    instructions.foldLeft(start) { (pos, instruction) =>
      moveCube(cubeMap, pos, instruction, size, connections)
    }

  def password(location: Location): Int =
    // offset positions, only relevant here
    1000 * (location.pos.y + 1) + 4 * (location.pos.x + 1) + FacingDirection.passwordValue(location.facingDirection)

  @main
  def solution1(): Unit =
    val start = startOf(input.areaMap)
    val finalLocation = iteratedMove(input.areaMap, start, input.instructions)
    val result = password(finalLocation)
    pprint.log(result)

  @main
  def solution2(): Unit =
    val sideSize = 4
    val schema = List(List(CubeSide.S0), List(CubeSide.S4, CubeSide.S3, CubeSide.S5), List(CubeSide.S2, CubeSide.S1))
    val cubeMap = cubeInput(sideSize, schema)
    val start = CubeLocation(
      cubeSide = CubeSide.S0,
      location = startOf(cubeMap(CubeSide.S0))
    )

    // Test
    val connections: Connections = Map(
      (CubeSide.S0, FacingDirection.L) -> (CubeSide.S3, FacingDirection.D),
      (CubeSide.S0, FacingDirection.R) -> (CubeSide.S1, FacingDirection.L),
      (CubeSide.S0, FacingDirection.U) -> (CubeSide.S4, FacingDirection.D),
      (CubeSide.S0, FacingDirection.D) -> (CubeSide.S5, FacingDirection.D),
      (CubeSide.S1, FacingDirection.L) -> (CubeSide.S2, FacingDirection.L),
      (CubeSide.S1, FacingDirection.R) -> (CubeSide.S0, FacingDirection.L),
      (CubeSide.S1, FacingDirection.U) -> (CubeSide.S5, FacingDirection.L),
      (CubeSide.S1, FacingDirection.D) -> (CubeSide.S4, FacingDirection.R),
      (CubeSide.S2, FacingDirection.L) -> (CubeSide.S3, FacingDirection.U),
      (CubeSide.S2, FacingDirection.R) -> (CubeSide.S1, FacingDirection.R),
      (CubeSide.S2, FacingDirection.U) -> (CubeSide.S5, FacingDirection.U),
      (CubeSide.S2, FacingDirection.D) -> (CubeSide.S4, FacingDirection.U),
      (CubeSide.S3, FacingDirection.L) -> (CubeSide.S4, FacingDirection.L),
      (CubeSide.S3, FacingDirection.R) -> (CubeSide.S5, FacingDirection.R),
      (CubeSide.S3, FacingDirection.U) -> (CubeSide.S0, FacingDirection.R),
      (CubeSide.S3, FacingDirection.D) -> (CubeSide.S2, FacingDirection.R),
      (CubeSide.S4, FacingDirection.L) -> (CubeSide.S1, FacingDirection.U),
      (CubeSide.S4, FacingDirection.R) -> (CubeSide.S3, FacingDirection.R),
      (CubeSide.S4, FacingDirection.U) -> (CubeSide.S0, FacingDirection.D),
      (CubeSide.S4, FacingDirection.D) -> (CubeSide.S2, FacingDirection.U),
      (CubeSide.S5, FacingDirection.L) -> (CubeSide.S3, FacingDirection.L),
      (CubeSide.S5, FacingDirection.R) -> (CubeSide.S1, FacingDirection.D),
      (CubeSide.S5, FacingDirection.U) -> (CubeSide.S0, FacingDirection.U),
      (CubeSide.S5, FacingDirection.D) -> (CubeSide.S2, FacingDirection.D)
    )
    // Real
//    val connections: Connections = Map(
//
//    )

    val finalLocation = iteratedMoveCube(cubeMap, start, input.instructions, connections)
    pprint.log(finalLocation)
    val (xOffset, yOffset) =
      schema
        .map(_.zipWithIndex)
        .zipWithIndex
        .flatMap { case (line, y) => line.map { case (s, x) => (x, y) -> s } }
        .find(_._2 == finalLocation.cubeSide)
        .get
        ._1
    val resetFinalPosition = finalLocation.location.copy(
      pos = finalLocation.location.pos.copy(
        x = finalLocation.location.pos.x + xOffset * sideSize,
        y = finalLocation.location.pos.y + yOffset * sideSize
      )
    )
    pprint.log(resetFinalPosition)
    val result = password(resetFinalPosition)
    pprint.log(result)

}
