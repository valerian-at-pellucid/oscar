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

package oscar.cp.modeling

import oscar.algo.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.collection.mutable.Stack
import oscar.algo.reversible._
import oscar.util._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint
import oscar.cp.core.CPOutcome._
import java.util.LinkedList
import java.util.Collection

class CPSolver(propagStrength: CPPropagStrength) extends CPOptimizer(propagStrength) {

  def this() = this(CPPropagStrength.Weak)

  private val decVariables = scala.collection.mutable.Set[CPIntVar]()

  var lastSol = new CPSol(Set[CPIntVar]())

  def addDecisionVariables(x: Iterable[CPIntVar]): Unit = x.foreach(decVariables += _)

  def addDecisionVariables(x: CPIntVar*): Unit = x.foreach(decVariables += _)

  private def recordSol(): Unit = {
    lastSol = new CPSol(decVariables.toSet)
  }

  private var throwNoSolExceptions = true

  /** Deactivate the no solution exception when an add is used and an inconsistent model is detected */
  def deactivateNoSolExceptions(): Unit = throwNoSolExceptions = false

  override def beforeStartAction(): Unit = deactivateNoSolExceptions()

  /**
   * return true if every variable is bound
   */
  def allBounds(vars: IndexedSeq[CPIntVar]): Boolean = {
    var i = 0
    val s = vars.size
    while (i < s) {
      if (!vars(i).isBound) return false
      i += 1
    }
    true
  }

  override def minimize(objective: CPIntVar): CPSolver = {
    super.minimize(Seq(objective): _*)
    this
  }
  
  override def minimize(objective: CPIntVar, ratio: Double): CPSolver = {
    super.minimize(objective, ratio)
    this
  }

  override def maximize(objective: CPIntVar): CPSolver = {
    super.maximize(Seq(objective): _*)
    this
  }

  override def update(): Unit = propagate()

  override def solFound(): Unit = {
    super.solFound()
    lastSol = new CPSol(decVariables.toSet)
    if (recordNonDominatedSolutions) {
      if (!silent) println("new solution:" + objective.objs.map(_.objVar.value).toArray.mkString(","))
      paretoSet.insert(lastSol, objective.objs.map(_.objVar.value): _*)
    }
    objective.tighten()
  }

  override def add(c: Constraint, st: CPPropagStrength): CPOutcome = {
    val outcome = post(c, st)
    if ((outcome == Failure || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $c")
    }
    outcome
  }

  override def add(c: Constraint): CPOutcome = add(c, propagStrength)

  /**
   * Add a constraint to the store (b == true) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
   */
  override def add(b: CPBoolVar): CPOutcome = {
    val outcome = post(new Eq(b, 1))
    if ((outcome == Failure || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when setting " + b.name + " to true")
    }
    return outcome
  }

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  override def add(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = {
    val outcome = post(constraints, st);
    if ((outcome == Failure || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $constraints");
    }
    return outcome
  }

  override def addCut(c: Constraint): CPOutcome = {
    val outcome = postCut(c)
    if ((outcome == Failure || isFailed) && throwNoSolExceptions) {
      throw new NoSolutionException(s"the stored failed when adding constraint $c")
    }
    outcome
  }

  override def add(constraints: Collection[Constraint]): CPOutcome = add(constraints, propagStrength)

  override def add(constraints: Iterable[Constraint], st: CPPropagStrength): CPOutcome = {
    val cs = new LinkedList[Constraint]()
    constraints.foreach(cs.add(_))
    add(cs, st)
  }

  override def add(constraints: Iterable[Constraint]): CPOutcome = add(constraints, propagStrength)

  override def +=(c: Constraint, st: CPPropagStrength): CPOutcome = add(c, st)
  override def +=(c: Constraint): CPOutcome = add(c, propagStrength)
}

object CPSolver {

  /** Creates a new CP Solver */
  def apply(): CPSolver = new CPSolver()

  /** Creates a new CP Solver with `propagStrength` as default level of propagation */
  def apply(propagStrength: CPPropagStrength) = new CPSolver(propagStrength)

  /** Creates a new CP Solver with `Weak` as default level of propagation */
  def weak: CPSolver = new CPSolver(CPPropagStrength.Weak)

  /** Creates a new CP Solver with `Medium` as default level of propagation */
  def medium: CPSolver = new CPSolver(CPPropagStrength.Medium)

  /** Creates a new CP Solver with `Strong` as default level of propagation */
  def strong: CPSolver = new CPSolver(CPPropagStrength.Strong)
}
