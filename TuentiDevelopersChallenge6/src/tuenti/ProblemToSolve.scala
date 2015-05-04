package tuenti

case class ProblemToSolve(y0: Int, x0: Int, y1: Int, x1: Int, k: Int) {
  val width = x1 - x0 + 1
  val height = y1 - y0 + 1
}

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    ProblemToSolve(ss(0).toInt, ss(1).toInt, ss(2).toInt, ss(3).toInt, ss(4).toInt)
  }
}
