package tuenti.model

/* In wonderful inmutable state fashion */

trait Action
case object PILLAGE extends Action
case object AUTO_MOVE extends Action
case class MOVE(target: Island, cost: Int) extends Action

case class Ship(number: Int, name: String, gold: Int, islandsVisited: List[Island], currentIsland: Island, movedLastTurn: Boolean) {

  override def toString = s"${name}(${gold}) @ ${islandsVisited.map(_.name).mkString("<-")}"

  def possibleActions = if (gold == 0) List(PILLAGE) else
    currentIsland.routes
      .filterNot(x => islandsVisited.contains(x._2))
      .filter(x => gold >= x._1.cost + x._2)
      .toList.map(x => MOVE(x._1, x._2)) :+ PILLAGE

  def doAction(action: Action): Ship = action match {
    case PILLAGE                         => pillage
    case AUTO_MOVE                       => moveAuto
    case MOVE(target: Island, cost: Int) => moveTo(target, cost)
  }

  private def moveTo(target: Island, cost: Int): Ship = {

    if (number == 1) {
      val newGold = gold - cost - target.cost

      if (newGold < 0)
        Ship(number, name, 0, target :: islandsVisited, target, true)
      else
        Ship(number, name, newGold, target :: islandsVisited, target, true)

    } else {
      Ship(number, name, gold, target :: islandsVisited, target, true)
    }

  }

  private def pillage: Ship = Ship(number, name, gold + 10, islandsVisited, currentIsland, false)

  def fight(rivals: List[Ship]): Ship = {

    val newGold = gold - rivals.foldLeft(0) {
      (sum: Int, rival: Ship) =>
        if (rival.movedLastTurn && rival.currentIsland == currentIsland) sum + rival.gold else sum
    }

    if (newGold < 0) Ship(number, name, 0, islandsVisited, currentIsland, movedLastTurn)
    else Ship(number, name, newGold, islandsVisited, currentIsland, movedLastTurn)

  }

  private def moveAuto: Ship = {

    val routeToTake = if (number % 2 == 0) currentIsland.routes.maxBy(_._2) else currentIsland.routes.minBy(_._2)

    if (routeToTake != null)
      moveTo(routeToTake._1, routeToTake._2)
    else
      Ship(number, name, gold, islandsVisited, currentIsland, false)
  }
}

object Ship {
  def apply(line: String): Ship = {
    val ss = line.split(" ")
    val startIsland = Island.find(ss(3))
    Ship(ss(0).toInt, ss(1), ss(2).toInt, startIsland :: List.empty, startIsland, false)
  }
}
