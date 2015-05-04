package tuenti

case class ProblemToSolve(N: Int, M: Int)

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    ProblemToSolve(ss(0).toInt, ss(1).toInt)
  }
}
