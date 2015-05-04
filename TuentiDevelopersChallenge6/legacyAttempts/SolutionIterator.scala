package tuenti

class SolutionIterator(val output: IndexedSeq[IndexedSeq[SumContainer]], val initialSkip: Int, N: Int, k: Int) {

  var skip = initialSkip
  var accept = N - 2 * k

  val lineIterator = output.iterator

  var columnIterator = lineIterator.next.iterator

  var done: Boolean = false

  private def doSum: Boolean = {
    if (skip > 0) {
      skip -= 1
      false
    } else if (done)
      false
    else true
  }

  def sum(i: Int) {

    if (doSum) {
      if (!columnIterator.hasNext) {
        if (!lineIterator.hasNext) {
          done = true
          return
        }
        columnIterator = lineIterator.next.iterator
      }

      columnIterator.next += i
      accept -= 1

      if (accept == 0) {
        skip = 2 * k
        accept = N - 2 * k
      }
    }
  }

}

object SolutionIterator {

  def initialize(k: Int, N: Int, output: IndexedSeq[IndexedSeq[SumContainer]]) = {

    val secondBlockSkip = (N + 1) * (k + 1)

    val it1 =
      (for {
        i <- 0 until k
        j <- 0 until k
      } yield new SolutionIterator(output, i + N * j, N, k))

    val it2 =

      (for {
        i <- 0 until k
        j <- 0 until k
      } yield new SolutionIterator(output, secondBlockSkip + i + N * j, N, k))

    it1 ++ it2

  }

}
