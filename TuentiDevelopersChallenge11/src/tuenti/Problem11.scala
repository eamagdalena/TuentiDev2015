package tuenti

import Utils._
import Scenario._

import scala.concurrent.Future

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Problem11 extends App {

  val scenariosToSolve = fromFile(args.head).toList.map { _.toInt }

  val scenarios = parseScenariosFile(fromFile("scenarios.txt"), scenariosToSolve.max).drop(scenariosToSolve.min)

  def solve(scenario: Scenario): BigInt = {

    println("Solving scenario " + scenario + "...")

    val simulation = new Simulator(Run(scenario))

    simulation.execute

    val result = simulation.result

    //    println(result.result())

    println("Computing score... " + scenario.number + "...")

    val res = result.result().sum % BigInt(1000000007)

    println("Score: " + res)

    res
  }

  //println(solve(scenarios(42)))

  val res = scenarios.map { scenario => s"Scenario ${scenario.number}: ${solve(scenario).toString}" }

  println(res)

  //  val resFutures = problemsToSolve.map(p => Future(solve(p)))
  //val res = Await.result(Future.sequence(resFutures), Duration(6000, "seconds"))

  toFile(pretty(res), s"${args.head}.solved")

}
