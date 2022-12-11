package day11

case class Monkey(
    items: List[BigInt],
    operation: BigInt => BigInt,
    divisibleBy: BigInt,
    onTrue: Int,
    onFalse: Int,
    inspected: BigInt = 0
)

object Monkey {

  def addItem(monkey: Monkey, item: BigInt): Monkey =
    monkey.copy(
      items = monkey.items :+ item
    )

  def clearAfterInspectionItems(monkey: Monkey): Monkey = {
    val number = monkey.items.size
    monkey.copy(
      items = List.empty,
      inspected = monkey.inspected + number
    )
  }

}
