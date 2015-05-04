package tuenti

import Utils._

case class ProblemToSolve(A: Long, B: Long)

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    ProblemToSolve(ss.head.toLong, ss.tail.head.toLong)
  }
}

object Problem2 extends App {

  val problemsToSolve = for (line <- fromFile(args.head).drop(1)) yield ProblemToSolve(line)

  /* We use a list with all primes up to 5 * 10^7. As worst case is that * 2 */
  /* NOTE: This list is not shipped with the code as it makes the source too big, and the submit form complains */
  val primes: List[Long] = fromFile("P-1-50000001.txt").map { x => x.split(", ").tail.head.toLong }

  def primesStream: Stream[List[Long]] = {
    def stream(l: List[Long]): Stream[List[Long]] = l #:: stream(l.tail)
    stream(primes)
  }

  val limit = problemsToSolve.maxBy { _.B }.B

  val stream = primesStream.takeWhile { x => x.head * x.head <= limit }

  val semiPrimes = stream.flatMap { l => l.iterator.takeWhile { x => x * l.head <= limit }.map { x => l.head * x } }.toList

  val solution = for (p <- problemsToSolve) yield semiPrimes.count { x => x >= p.A && x <= p.B }

  toFile(pretty(solution), s"${args.head}.solved")

}
