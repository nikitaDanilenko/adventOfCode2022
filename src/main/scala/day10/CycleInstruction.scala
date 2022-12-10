package day10

sealed trait CycleInstruction

object CycleInstruction {
  case object Wait extends CycleInstruction
  case class AddValue(value: Int) extends CycleInstruction
}
