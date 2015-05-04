package tuenti

import Utils._

import scala.concurrent.Future

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Problem6 extends App {

  val problemsToSolve = for (line <- fromFile(args.head).drop(1)) yield ProblemToSolve(line)

  //println("Loading Big Sheet")
  val sheet: List[List[Long]] = fromFile("sheet.data").tail.map { x => x.split(" ").toList.map(i => i.toLong) }

  def magicStream(list: List[Long], k: Int): Stream[Long] = {
    if (list.length < k) Stream.Empty else
      list.iterator.take(k).sum #:: magicStream(list.tail, k) // iterator?
  }

  def precalculateSheet(p: ProblemToSolve) = {
    //println("Precalculating sheet for " + p)

    val shredeedSheet = sheet.take(p.y1 + 1).drop(p.y0).map { x => x.take(p.x1 + 1).drop(p.x0) }

    val preComputedSheetFuture = shredeedSheet.map { l => Future(magicStream(l, p.k).toList) }
    val preComputedSheet = Await.result(Future.sequence(preComputedSheetFuture), Duration(6000, "seconds")).transpose

    val magicalSheetFutures = preComputedSheet.map { l => Future(magicStream(l, p.k).toList) }
    Await.result(Future.sequence(magicalSheetFutures), Duration(6000, "seconds")).transpose

  }

  def solve(p: ProblemToSolve, sheet: List[List[Long]]): Long = {
    //println("Running magic calculus on problem " + p + ". Size is " + (2 * p.k * p.width * p.height))

    val iterator1 = sheet.iterator
    val iterator2 = sheet.iterator.drop(p.k + 1)

    val solution = (
      for {
        y <- 0 until p.height - 2 * p.k

      } yield {

        val v1 = iterator1.next.take(p.width - 2 * p.k)
        val v2 = iterator2.next.drop(p.k + 1)

        (v1, v2).zipped.map(_ + _).max

      }).max

    //println("Solution for " + p + " is " + solution)

    solution
  }

  val resFutures = problemsToSolve.map(p => {
    val sheet = precalculateSheet(p)
    Future(solve(p, sheet))
  })

  val res = Await.result(Future.sequence(resFutures), Duration(6000, "seconds"))

  println(res)

  val output = (for (i <- 1 to res.length) yield "Case " + i + ": " + res(i - 1)).toList

  toFile(pretty(output), s"${args.head}.solved")

}
