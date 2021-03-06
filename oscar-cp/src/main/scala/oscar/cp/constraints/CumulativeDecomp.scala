package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPSolver

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CumulativeDecomp(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) extends Constraint(starts.head.store, "Cumulative decomposition") {

  private val maxEndDate = maxValue(ends)
  private val minEndDate = minValue(starts)
  private val Horizon = minEndDate to maxEndDate

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  private def overlapingTasks(t: Int): IndexedSeq[Int] = {
    Tasks.filter(task => starts(task).min <= t && ends(task).max > t)
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    for (t <- Horizon) {
      val tasks = overlapingTasks(t)
      val overlapingVars = tasks.map(task => ((starts(task) <== t) and (ends(task) >>= t) and (resources(task) === id)) * demands(task))
      val minDemand = minSum(overlapingVars)
      val maxDemand = maxSum(overlapingVars)
      val totDemand = CPIntVar(minDemand to maxDemand)(s)
      if (s.post(new Sum(overlapingVars.toArray, totDemand)) == Failure) return Failure
      if (s.post(totDemand <= capacity.max) == Failure) return Failure
    }
    Suspend
  }

  private def maxValue(vars: Seq[CPIntVar]): Int = {
    var max = vars.head.max
    vars.tail.foreach(x => max = math.max(max, x.max))
    max
  }

  private def minValue(vars: Seq[CPIntVar]): Int = {
    var min = vars.head.min
    vars.tail.foreach(x => min = math.min(min, x.min))
    min
  }

  private def minSum(vars: Seq[CPIntVar]): Int = {
    var sum = 0
    vars.foreach(sum += _.min)
    sum
  }

  private def maxSum(vars: Seq[CPIntVar]): Int = {
    var sum = 0
    vars.foreach(sum += _.max)
    sum
  }
}