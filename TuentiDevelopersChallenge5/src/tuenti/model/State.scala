package tuenti.model

trait Result

case object GAME_OVER extends Result

case class WINNER(bounty: Int) extends Result

case object CONTINUE extends Result

case class State(myShip: Ship, rivalShips: List[Ship]) {

  def performAction(action: Action) = {

    val myShipAfterAction = myShip.doAction(action)

    val rivalsAfterMove = rivalShips.map { _.doAction(AUTO_MOVE) }

    if (myShipAfterAction.currentIsland != Island.treasureIsland)
      State(myShipAfterAction.fight(rivalsAfterMove), rivalsAfterMove)
    else
      State(myShipAfterAction, rivalsAfterMove)
  }

  def checkStatus: Result = {
    if (myShip.currentIsland == Island.treasureIsland)
      WINNER(myShip.gold)
    else if (rivalShips.exists { _.currentIsland == Island.treasureIsland })
      GAME_OVER
    else
      CONTINUE
  }

}
