package tuenti

import Utils._

import tuenti.model._

object Problem5 extends App {

  var input = fromFile(args.head)

  val numberIslands = input.head.toInt

  val islands = input.tail.take(numberIslands).map { Island(_) }

  input = input.drop(numberIslands + 1)

  val numberRoutes = input.head.toInt

  val routes = input.tail.take(numberRoutes).map { Route(_) }

  input = input.drop(numberRoutes + 2)

  val myShip = Ship(input.head)
  val rivalShips = input.tail.map { Ship(_) }

  def solve(state: State): Int = {

    state.checkStatus match {

      case GAME_OVER           => 0

      case WINNER(bounty: Int) => bounty

      case CONTINUE => {
        state.myShip.possibleActions.map { x => solve(state.performAction(x)) }.max
      }

    }

  }

  val solution = solve(State(myShip, rivalShips))

  println(solution)

  toFile(solution.toString, s"${args.head}.solved")

}
