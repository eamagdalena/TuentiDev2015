package tuenti

import Utils._
import XCorrelation._

import scala.concurrent.Future

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Problem9 extends App {

  val input = fromFile(args.head)

  val p = ProblemToSolve(input.next)

  val pattern = (for (i <- 1 to p.P) yield input.next.toDouble).toList
  input.drop(1)
  val wave = (for (i <- 1 to p.W) yield input.next.toDouble).toList

  val xCorrelator = XCorrelation(wave, pattern)

  val res = "%.4f".format(xCorrelator.findScore)

  println(res)

  //  val resFutures = problemsToSolve.map(p => Future(solve(p)))
  //val res = Await.result(Future.sequence(resFutures), Duration(6000, "seconds"))

  toFile(res, s"${args.head}.solved")

}
