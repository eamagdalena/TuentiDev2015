package tuenti

import scala.collection.mutable.HashMap

object CachedFactorial {

  lazy val factorials: IndexedSeq[BigInt] = {

    println("Precomputing factorials...")

    var previous: BigInt = 1

    for {
      i <- (1 to 1000)
    } yield {
      val res = previous * i
      previous = res
      res
    }

  }

  def factorial(n: Int): BigInt = if (n == 0) 1 else factorials(n - 1)

}
