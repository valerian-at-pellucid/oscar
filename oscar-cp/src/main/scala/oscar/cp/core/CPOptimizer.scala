package oscar.cp.core

import oscar.cp.modeling.CPSolver
import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.constraints.ParetoConstraint
import oscar.cp.multiobjective.Pareto
import oscar.cp.multiobjective.ListPareto
import oscar.cp.constraints.CPObjectiveUnitMaximize
import oscar.cp.constraints.CPObjective
import oscar.cp.constraints.CPObjectiveUnitMinimize
import oscar.cp.modeling.TightenType
import oscar.cp.constraints.CPObjectiveGeometricMinimize
import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.constraints.CPObjectiveUnit

class CPOptimizer(propagStrength: CPPropagStrength) extends CPStore(propagStrength) {
  
  def this() = this(CPPropagStrength.Weak)
  
  var objective = new CPObjective(this, Array[CPObjectiveUnit]());
  
  private val decVariables = scala.collection.mutable.Set[CPIntVar]()
  
  var paretoSet: Pareto[CPSol] = new ListPareto[CPSol](Array())
  var recordNonDominatedSolutions = false
  
  def nonDominatedSolutions: Seq[CPSol] = paretoSet.toList
  def nonDominatedSolutionsObjs: Seq[IndexedSeq[Int]] = paretoSet.objectiveSols 
  
  def obj(objVar: CPIntVar): CPObjectiveUnit = objective(objVar)

  def optimize(obj: CPObjective): CPOptimizer = {
    objective = obj
    postCut(obj)
    this
  }

  def minimize(objective: CPIntVar): CPOptimizer = minimize(Seq(objective): _*)
  
  def minimize(objectives: CPIntVar*): CPOptimizer = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMinimize(_)): _*))
  
  def minimize(objective: CPIntVar, ratio: Double): CPOptimizer = {
    val o = new CPObjectiveGeometricMinimize(objective, "GeometricMinimize", ratio): CPObjectiveUnit
    optimize(new CPObjective(this, Array(o)))
  }
  
  def maximize(objective: CPIntVar): CPOptimizer = maximize(Seq(objective): _*)

  def maximize(objectives: CPIntVar*): CPOptimizer = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMaximize(_)): _*))
  
  def paretoMinimize(objective: CPIntVar): CPOptimizer = paretoOptimize((objective, false))  
  
  def paretoMinimize(objectives: CPIntVar*): CPOptimizer = paretoOptimize(objectives.map((_, false)): _*)
  
  def paretoMaximize(objective: CPIntVar): CPOptimizer = paretoOptimize((objective, true))
  
  def paretoMaximize(objectives: CPIntVar*): CPOptimizer = paretoOptimize(objectives.map((_, true)): _*)
  
  def paretoOptimize(objVarModes: (CPIntVar, Boolean)): CPOptimizer = paretoOptimize(Seq(objVarModes): _*)
  
  def paretoOptimize(objVarMode: (CPIntVar, Boolean)*): CPOptimizer = {
    
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
    
    paretoSet = new ListPareto[CPSol](isMax)
    // Adds a dominance constraint with all objectives in minimization mode
    addCut(new ParetoConstraint(paretoSet, isMax, objectives.toArray))
    this
  }
  
  @deprecated("solve is the default behavior of CPSolver and does not need to be specified anymore.", "1.0")
  def solve(): CPOptimizer = this

  @deprecated("constraints do not need to be stated in the subjectTo block anymore.", "1.0")
  def subjectTo(constraintsBlock: => Unit): CPOptimizer = {
    try {
      constraintsBlock
    } catch {
      case ex: NoSolutionException => println("No Solution, inconsistent model")
    }
    this
  }
  
  override def update(): Unit = propagate()
}