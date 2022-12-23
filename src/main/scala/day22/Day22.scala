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
      .fromResource("test/day22.txt")
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

  def moveCube(
      cubeMap: CubeMap,
      cubeLocation: CubeLocation,
      instruction: Instruction,
      cubeSize: Int,
      connections: Connections
  ): CubeLocation = {
    lazy val xs = cubeMap(cubeLocation.cubeSide).collect {
      case (pos, _) if pos.y == cubeLocation.location.pos.y => pos.x
    }
    lazy val ys = cubeMap(cubeLocation.cubeSide).collect {
      case (pos, _) if pos.x == cubeLocation.location.pos.x => pos.y
    }
    instruction match
      case Instruction.Move(units) =>
        cubeLocation.location.facingDirection match
          case FacingDirection.L =>
            1.to(units).foldLeft(cubeLocation) { (cl, _) =>
              val newLocation =
                if cl.location.pos.x - 1 >= 0 then
                  cl.copy(
                    location = cl.location.copy(
                      pos = cl.location.pos.copy(x = cl.location.pos.x - 1)
                    )
                  )
                else {
                  val (neighbourSide, direction) = connections(cl.cubeSide, cubeLocation.location.facingDirection)
                  val newPos = direction match
                    case FacingDirection.L =>
                      Pos(
                        x = cubeSize,
                        y = cl.location.pos.y
                      )
                    case FacingDirection.R =>
                      Pos(
                        x = 0,
                        y = ys.max - cl.location.pos.y
                      )
                    case FacingDirection.U =>
                      Pos(
                        x = cubeSize - cl.location.pos.y,
                        y = ys.max
                      )
                    case FacingDirection.D =>
                      Pos(
                        x = cl.location.pos.y,
                        y = ys.min
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
            }
          case FacingDirection.R =>
            1.to(units).foldLeft(cubeLocation) { (cl, _) =>
              val newLocation =
                if cl.location.pos.x + 1 <= cubeSize then
                  cl.copy(
                    location = cl.location.copy(
                      pos = cl.location.pos.copy(x = cl.location.pos.x + 1)
                    )
                  )
                else {
                  val (neighbourSide, direction) = connections(cl.cubeSide, cubeLocation.location.facingDirection)
                  val newPos = direction match
                    case FacingDirection.L =>
                      Pos(
                        x = cubeSize,
                        y = cubeSize - cl.location.pos.y
                      )
                    case FacingDirection.R =>
                      Pos(
                        x = 0,
                        y = cl.location.pos.y
                      )
                    case FacingDirection.U =>
                      Pos(
                        x = cl.location.pos.y,
                        y = cubeSize
                      )
                    case FacingDirection.D =>
                      Pos(
                        x = cubeSize - cl.location.pos.y,
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
            }
          case FacingDirection.U =>
            1.to(units).foldLeft(cubeLocation) { (cl, _) =>
              val newLocation =
                if cl.location.pos.y - 1 >= 0 then
                  cl.copy(
                    location = cl.location.copy(
                      pos = cl.location.pos.copy(y = cl.location.pos.y - 1)
                    )
                  )
                else {
                  val (neighbourSide, direction) = connections(cl.cubeSide, cubeLocation.location.facingDirection)
                  val newPos = direction match
                    case FacingDirection.L =>
                      Pos(
                        x = 0,
                        y = cl.location.pos.x
                      )
                    case FacingDirection.R =>
                      Pos(
                        x = cubeSize,
                        y = cubeSize - cl.location.pos.x
                      )
                    case FacingDirection.U =>
                      Pos(
                        x = cl.location.pos.x,
                        y = cubeSize
                      )
                    case FacingDirection.D =>
                      Pos(
                        x = cubeSize - cl.location.pos.x,
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
            }
          case FacingDirection.D =>
            1.to(units).foldLeft(cubeLocation) { (cl, _) =>
              val newLocation =
                if cl.location.pos.y + 1 <= 0 then
                  cl.copy(
                    location = cl.location.copy(
                      pos = cl.location.pos.copy(y = cl.location.pos.y + 1)
                    )
                  )
                else {
                  val (neighbourSide, direction) = connections(cl.cubeSide, cubeLocation.location.facingDirection)
                  val newPos = direction match
                    case FacingDirection.L =>
                      Pos(
                        x = cubeSize,
                        y = cl.location.pos.x
                      )
                    case FacingDirection.R =>
                      Pos(
                        x = 0,
                        y = cubeSize - cl.location.pos.x
                      )
                    case FacingDirection.U =>
                      Pos(
                        x = cubeSize - cl.location.pos.x,
                        y = cubeSize
                      )
                    case FacingDirection.D =>
                      Pos(
                        x = cl.location.pos.x,
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
    instructions.foldLeft(start)(moveCube(cubeMap, _, _, size, connections))

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
    val ci =
      cubeInput(4, List(List(CubeSide.S0), List(CubeSide.S4, CubeSide.S3, CubeSide.S5), List(CubeSide.S2, CubeSide.S1)))
    pprint.log(ci)

}
