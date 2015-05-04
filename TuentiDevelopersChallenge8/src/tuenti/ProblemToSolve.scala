package tuenti

case class ProblemToSolve(items: List[BookEntry])

object ProblemToSolve {
  def apply(line: String): ProblemToSolve = {
    val ss = line.split(" ")
    new ProblemToSolve(ss.map(BookEntry.find).toList)
  }
}
