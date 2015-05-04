package tuenti

import Utils._
import scala.concurrent.Future

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

case class ProblemToSolve(A: Int, B: Int)

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    ProblemToSolve(ss.head.toInt, ss.tail.head.toInt)
  }
}

object Problem3 extends App {

  val primes: List[Int] = fromFile("P-25.txt").map { x => x.split(", ").tail.head.toInt }

  val problemsToSolve = for (line <- fromFile(args.head).drop(1)) yield ProblemToSolve(line)

  /* This will help us to to avoid unnecesary calculations */
  val upperLimit = problemsToSolve.maxBy { _.B }.B
  val lowerLimit = problemsToSolve.minBy { _.A }.A

  /* numbers.txt must be copied to the working directory as it was not submited with the code, because of size issues */
  val products: List[BigInt] = fromFile("numbers.txt").drop(lowerLimit).take(upperLimit - lowerLimit).map { x => BigInt(x) }

  val decomposedProducts = products.map { decompose }

  /** Decomposes a BigInt by its prime factors: Very fast */
  def decompose(b: BigInt): List[Int] = {

    def decompose(b: BigInt, prime: Int): List[Int] = {

      if (b % prime == 0) prime :: decompose(b / prime, prime)
      else Nil
    }

    (for (p <- primes) yield decompose(b, p)).flatten

  }

  /** Given all the prime factors of all the "stone", cut the slice we want, flatten it in one list (slowww), count every factor, find the max count and return all maxed keys */
  def solve(p: ProblemToSolve): String = {

    val descompositions = decomposedProducts
      .drop(p.A - lowerLimit)
      .take(p.B - p.A)
      .flatten
      .groupBy(x => x)
      .mapValues(x => x.size)

    val max = descompositions.maxBy(_._2)._2

    val y = descompositions.filter(_._2 == max).keys.toList.sorted

    s"${max} ${y.mkString(" ")}"
  }

  /* Use futures to finish faster */
  val solutionsFuture = for {
    p <- problemsToSolve
  } yield Future(solve(p))

  val res = Await.result(Future.sequence(solutionsFuture), Duration(6000, "seconds"))

  toFile(pretty(res), s"${args.head}.solved")

}
