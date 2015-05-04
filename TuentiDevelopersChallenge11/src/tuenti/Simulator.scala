package tuenti

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.VectorBuilder

class Simulator(initialRun: Run) {

  val result = new VectorBuilder[BigInt] // 8 seconds on case 50

  val pendingRuns = new scala.collection.mutable.Stack[Run]

  pendingRuns.push(initialRun)

  def execute() { /* We optimize like this to avoid running out of memory on large recursions */

    while (!pendingRuns.isEmpty) {
      internalExecute(pendingRuns.pop)
    }

  }

  private def internalExecute(run: Run) {

    if (run.exited) {
      result += run.deadSet.reduce(_ * _)
    } else if (run.location == null) {
      //DO NOTHING
    } else {
      val actions = run.possibleActions

      if (!actions.isEmpty) { /* dead end */
        actions.map { action => run.doAction(action) }.foreach { r => pendingRuns.push(r) }
      }
    }
  }

}
