package day22

import cats.parse.Parser

enum FacingDirection:
  case L, R, U, D

object FacingDirection {

  def rotate(facingDirection: FacingDirection, rotatingDirection: RotatingDirection): FacingDirection =
    (facingDirection, rotatingDirection) match
      case (FacingDirection.L, RotatingDirection.L) => FacingDirection.D
      case (FacingDirection.L, RotatingDirection.R) => FacingDirection.U
      case (FacingDirection.R, RotatingDirection.L) => FacingDirection.U
      case (FacingDirection.R, RotatingDirection.R) => FacingDirection.D
      case (FacingDirection.U, RotatingDirection.L) => FacingDirection.L
      case (FacingDirection.U, RotatingDirection.R) => FacingDirection.R
      case (FacingDirection.D, RotatingDirection.L) => FacingDirection.R
      case (FacingDirection.D, RotatingDirection.R) => FacingDirection.L

  def passwordValue(facingDirection: FacingDirection): Int =
    facingDirection match
      case FacingDirection.L => 2
      case FacingDirection.R => 0
      case FacingDirection.U => 3
      case FacingDirection.D => 1

}
