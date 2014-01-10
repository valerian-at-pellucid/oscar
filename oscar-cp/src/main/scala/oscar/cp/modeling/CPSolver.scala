/*******************************************************************************
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
 ******************************************************************************/

package oscar.cp.modeling

import oscar.algo.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.util.continuations._
import scala.collection.mutable.Stack
import oscar.cp.scheduling.CumulativeActivity
import oscar.algo.reversible._
import oscar.util._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint


class CPSolver() extends CPStore() {

  def +=(cons: Constraint, propagStrength: CPPropagStrength = CPPropagStrength.Weak): Unit = {
    this.add(cons, propagStrength)
  }
  
  var objective = new CPObjective(this, Array[CPObjectiveUnit]());
  

  
  private val decVariables = scala.collection.mutable.Set[CPVarInt]()
  var lastSol = new CPSol(Set[CPVarInt]())
  var paretoSet: Pareto[CPSol] = new ListPareto[CPSol](Array())
  var recordNonDominatedSolutions = false
  
  def nonDominatedSolutions: Seq[CPSol] = paretoSet.toList
  def nonDominatedSolutionsObjs: Seq[IndexedSeq[Int]] = paretoSet.objectiveSols 
  
  def addDecisionVariables(x: Iterable[CPVarInt]) {
    x.foreach(decVariables += _)
  }
  
  	def addDecisionVariables(x: CPVarInt*) {
    x.foreach(decVariables += _)
  }
  
  private def recordSol() {
    lastSol = new CPSol(decVariables.toSet)
  }
  
  def obj(objVar: CPVarInt): CPObjectiveUnit = {
    objective(objVar)
  }

  def optimize(obj: CPObjective): CPSolver = {
    objective = obj
    postCut(obj)
    this
  }

  def minimize(objective: CPVarInt): CPSolver = minimize(Seq(objective): _*)
  
  def minimize(objectives: CPVarInt*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMinimize(_)): _*))

  def maximize(objective: CPVarInt): CPSolver = maximize(Seq(objective): _*)

  def maximize(objectives: CPVarInt*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMaximize(_)): _*))
  
  def paretoMinimize(objective: CPVarInt): CPSolver = paretoOptimize((objective, false))  
  
  def paretoMinimize(objectives: CPVarInt*): CPSolver = paretoOptimize(objectives.map((_, false)): _*)
  
  def paretoMaximize(objective: CPVarInt): CPSolver = paretoOptimize((objective, true))
  
  def paretoMaximize(objectives: CPVarInt*): CPSolver = paretoOptimize(objectives.map((_, true)): _*)
  
  def paretoOptimize(objVarModes: (CPVarInt, Boolean)): CPSolver = paretoOptimize(Seq(objVarModes): _*)
  
  def paretoOptimize(objVarMode: (CPVarInt, Boolean)*): CPSolver = {
    
    // objVar of each objective
    val objectives = objVarMode.map(_._1).toArray
    // true if objective i has to be maximized
    val isMax = objVarMode.map(_._2).toArray

    recordNonDominatedSolutions = true
    objective = new CPObjective(this, (for (i <- 0 until isMax.size) yield {
      if (isMax(i)) new CPObjectiveUnitMaximize(objectives(i))
      else new CPObjectiveUnitMinimize(objectives(i))
    }): _*)
    postCut(objective)
    objective.objs.foreach(_.tightenMode = TightenType.NoTighten)
    
    
    addDecisionVariables(objectives)
    paretoSet = new ListPareto[CPSol](isMax)
    // Adds a dominance constraint with all objectives in minimization mode
    addCut(new ParetoConstraint(paretoSet, isMax, objectives.toArray))
    this
  }

  def solve(): CPSolver = {
    this
  }

  def subjectTo(constraintsBlock: => Unit): CPSolver = {
    try {
      constraintsBlock
    } catch {
      case ex: NoSolutionException => println("No Solution, inconsistent model")
    }
    this
  }
  
  override def beforeStartAction() {
    deactivateNoSolExceptions()
  }

  /**
   * return true if every variable is bound
   */
  def allBounds(vars: IndexedSeq[CPVarInt]): Boolean = {
    //vars.map(_.isBound).foldLeft(true)((a, b) => a & b)
	var i = 0 
	val s = vars.size
	while (i < s) {
	  if (!vars(i).isBound) return false
	  i += 1
	}
    true
    
  }
  
  override def update() = propagate()
  override def solFound() = {
    super.solFound()
    lastSol = new CPSol(decVariables.toSet)
    if (recordNonDominatedSolutions) {
      if (!silent) println("new solution:"+objective.objs.map(_.objVar.value).toArray.mkString(","))
      paretoSet.insert(lastSol, objective.objs.map(_.objVar.value):_*)
    }
    objective.tighten()
  }

}

object CPSolver {

  /**
   * Creates a new CP Solver
   */
  def apply(): CPSolver = {
    new CPSolver()
  }
}
