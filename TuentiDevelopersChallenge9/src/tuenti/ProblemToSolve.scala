package tuenti

case class ProblemToSolve(P: Int, W: Int)

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    new ProblemToSolve(ss(0).toInt, ss(1).toInt)
  }
}
