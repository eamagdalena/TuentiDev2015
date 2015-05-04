package tuenti

import Utils._

import scala.concurrent.Future
import scala.collection.mutable.ListBuffer

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Problem8 extends App {

  val book = (for (line <- fromFile("book.data")) yield BookEntry(line)).toList

  val problemsToSolve = (for (line <- fromFile(args.head).drop(1)) yield ProblemToSolve(line)).toList

  def solve(p: ProblemToSolve): Long = {

    def solveRec(items: List[BookEntry]): Long = {

      val valueItems = items.foldLeft(0L)(_ + _.value)

      lazy val distinctItems = items.toSet

      lazy val distinctItemsWithCount = distinctItems.map { x => (x, items.count { x == _ }) }

      val children =
        (for {
          possibleItemToBuild <- items.flatMap(_.buildsInto).toSet

          // if (!distinctItems.contains(possibleItemToBuild)) ???

          if (possibleItemToBuild.distinctIngredientsWithCounts.forall { ingAndCount => ingAndCount._2 <= items.count(ingAndCount._1 == _)
          })

        } yield {

          val lb: ListBuffer[BookEntry] = ListBuffer(possibleItemToBuild)
          lb ++= items
          lb --= possibleItemToBuild.ingredients

          solveRec(lb.toList)
        })

      if (children.isEmpty) valueItems
      else {
        val maxChildren = children.max
        if (maxChildren > valueItems) maxChildren else valueItems
      }

    }

    solveRec(p.items)
  }

  val resFutures = problemsToSolve.map(p => Future(solve(p)))
  val res = Await.result(Future.sequence(resFutures), Duration(6000, "seconds"))

  //println(res)

  toFile(pretty(res), s"${args.head}.solved")

}
