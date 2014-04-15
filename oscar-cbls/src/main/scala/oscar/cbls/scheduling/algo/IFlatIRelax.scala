package oscar.cbls.scheduling.algo

/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

import oscar.cbls.invariants.core.computation.Solution
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.scheduling.model.Activity
import oscar.cbls.scheduling.model.Planning
import oscar.cbls.scheduling.model.PrecedenceCleaner
import oscar.cbls.scheduling.model.Resource
import oscar.cbls.search.SearchEngine

/*
abstract class StopCriterion(){

  //This method is called by the iFlatRelax at each step where the model is feasible
  def shouldStop(it:Int,p:Planning):Boolean
}

case class plateauMaxItStopCriterion(maxIt: Int, stable: Int, verbose:Boolean)
  extends StopCriterion{

  var plateauLength = 0
  var bestMakeSpan = Int.MaxValue

  def shouldStop(it:Int,p:Planning):Boolean = {
    if (p.makeSpan.value < bestMakeSpan) {
      bestMakeSpan = p.makeSpan.value
      plateauLength = 0
      if (verbose) println("Better MakeSpan found: " + bestMakeSpan)
    } else {
      plateauLength += 1
    }

    if (it >= maxIt){
      if (verbose) println("STOP criterion: maximum iteration number reached.")
      return true
    }
    if (plateauLength >= stable){
      if (verbose) println("STOP criterion: " + stable + " iterations without improvement.")
      return true
    }
    return false
  }
}
*/

/**
 * @param p
 * @param verbose
 * @author renaud.delandtsheer@cetic.be
 */
class IFlatIRelax(p: Planning,
                  verbose: Boolean = true,
                  nbRelax: Int = 4,
                  pkillPerRelax: Int = 50) extends SearchEngine {
  val model: Store = p.model

  var it: Int = 0


  /**
   * This solves the jobshop by iterative relaxation and flattening
   * @param maxIt the max number of iterations of the search
   * @param stable the number of no successive noimprove that will cause the search to stop
   */
  def solve(maxIt: Int, stable: Int) {

    flattenWorseFirst()

    var bestSolution: Solution = model.solution(true)
    var bestMakeSpan = p.makeSpan.value
    var plateaulength = 0

    if (verbose) {
      println("----------------")
    }

    while (it < maxIt && plateaulength < stable) {
      //iterative weakening and flattening
      it += 1

      if (plateaulength > 10 && (plateaulength % 50) == 0) {

        jumpAndFlatten()

      } else {
        if(!relaxAndFlatten){
          if (verbose) println("STOP criterion: no relaxation could be achieved.")
          return
        }
      }

      if (verbose) {
        println(p.makeSpan)
        println("iteration: " + it)
      }

      if (p.makeSpan.value < bestMakeSpan) {
        bestSolution = model.solution(true)
        bestMakeSpan = p.makeSpan.value
        plateaulength = 0
        if (verbose) println("Better MakeSpan found")
        p.updateVisual()
      } else {
        plateaulength += 1
        p.clean()
      }

      if (verbose) println("----------------")
    }

    if (verbose) {
      if (it >= maxIt)
        println("STOP criterion: maximum iteration number reached.")
      if (plateaulength >= stable)
        println("STOP criterion: " + stable + " iterations without improvement.")
    }

    model.restoreSolution(bestSolution)

    p.clean()

    if (verbose) println("restored best solution")
  }


  /** performs a large set of relaxation to actually get away
    * from a seemignly exhausted search zone
    *
    */
  def jumpAndFlatten(){
    if (verbose) println("jumping****************")
    for (i <- 0 until nbRelax * 3) { relax(pkillPerRelax); }
    flattenWorseFirst()
  }

  /** performs the relaxation, followed by the flattening
    *
    * @return true if the planning is actually solid, that is: no relaxation can be performed.
    */
  def relaxAndFlatten():Boolean = {
    if (!relaxUntilMakespanReduced(pkillPerRelax, nbRelax)) {
      println("STOP criterion: no relaxation could be achieved.")
      return true
    }

    flattenWorseFirst()
    return false
  }

  /**
   * performs the relaxation of the critical path
   * @param pKill: the probability to kill a killable precedence constraint in percent
   * @return true if something could be relaxed, false if makespan is solid (made only of dependencies that cannot be relaxed)
   */
  def relax(pKill: Int): Boolean = {

    val potentiallykilledNodes = CriticalPathFinder.nonSolidCriticalPath(p)
    if (potentiallykilledNodes.isEmpty) return false

    for ((from, to) <- potentiallykilledNodes) {
      if (flip(pKill)) {
        doRelax(from,to,verbose)
      }
    }
    true
  }

  def doRelax(from:Activity, to:Activity, verbose:Boolean ){
    to.removeDynamicPredecessor(from, verbose)
  }

  /**
   * performs the relaxation of the critical path
   * @param pKill: the probability to kill a killable precedence constraint in percent
   * @param min: the minimal number of relaxation
   * @return true if something could be relaxed, false if makespan is solid
   */
  def relaxUntilMakespanReduced(pKill: Int, min: Int = 3): Boolean = {
    val m = p.makeSpan.value
    var n = 0
    var SomethingCouldBeRelaxed = false
    while ((p.makeSpan.value == m) || (n < min)) {
      n += 1
      if (relax(pKill)) {
        SomethingCouldBeRelaxed = true
      } else {
        if (verbose) println("Could not relax anymore (after " + (n - 1) + " relaxations.")
        return SomethingCouldBeRelaxed
      }
    }
    if (verbose) println("relaxed " + n + " times to shorten makespan")
    return SomethingCouldBeRelaxed
  }

  /**implements the standard flatten procedure*/
  def flattenWorseFirst() {
    var iterations = 0
    while (!p.worseOvershotResource.value.isEmpty) {
      if (iterations > p.activityCount)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. Check there is no conflict between non moveable activities.")
      iterations += 1

      val r: Resource = p.resourceArray(selectFrom(p.worseOvershotResource.value))

      val t: Int = r.worseOverShootTime

      val conflictActivities = r.conflictingActivities(t)
      val baseForEjection = r.baseActivityForEjection(t)

      selectMin2(baseForEjection, conflictActivities,
        estimateMakespanExpansionForNewDependency,
        p.canAddPrecedenceAssumingResourceConflict) match {
        case (a, b) =>
          b.addDynamicPredecessor(a, verbose)
        case null =>

          //no precedence can be added because some additional precedence must be killed to allow that
          //this happens when superTasks are used, and when dependencies have been added around the start and end tasks of a superTask
          //we search which dependency can be killed in the conflict set,
          val conflictActivityArray = conflictActivities.toArray
          val baseForEjectionArray = baseForEjection.toArray

          val dependencyKillers: Array[Array[PrecedenceCleaner]] =
            Array.tabulate(baseForEjection.size)(
              t1 => Array.tabulate(conflictActivityArray.size)(
                t2 => p.getDependencyToKillToAvoidCycle(baseForEjectionArray(t1), conflictActivityArray(t2))))

          selectMin2(baseForEjectionArray.indices, conflictActivityArray.indices,
            (a: Int, b: Int) => estimateMakespanExpansionForNewDependency(baseForEjectionArray(a), conflictActivityArray(b)),
            (a: Int, b: Int) => dependencyKillers(a)(b).canBeKilled) match {
            case (a, b) => {
                  if (verbose) println("need to kill dependencies to complete flattening")
              dependencyKillers(a)(b).killDependencies(verbose)

              conflictActivityArray(b).addDynamicPredecessor(baseForEjectionArray(a), verbose)
            }
            case null => throw new Error("cannot flatten at time " + t + " activities: " + conflictActivities)
          }

      }
    }
  }


  /** This computes an estimate of the Makespan expansion if the given precedence is added.
    * this estmate is completely wrong in itself, as a constant factor is added to each estimate.
    * since it is the same factor, you can use this method to chose among a set of precedence
    * because this will forget about the correcting factor.
    * @param from
    * @param to
    * @return
    */
  def estimateMakespanExpansionForNewDependency(from:Activity, to:Activity):Int = {
    from.earliestEndDate.value - to.latestStartDate.value
  }

}
