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

package oscar.linprog.modeling

import gurobi._
import scala.collection.mutable.{ ListBuffer, HashMap }
import oscar.algebra._

/**
 * @author Hrayr Kostanyan Hrayr.Kostanyan@ulb.ac.be, Pierre Schaus pschaus@gmail.com, Jean van der Vaeren
 */
class GurobiLP extends AbstractLP {
  println("GurobiLP")

  var nbRows = 0
  var nbCols = 0
  var sol = Array[Double]()
  var objectiveValue = 0.0
  var status = LPStatus.NOT_SOLVED
  var closed = false
  var released = false
  var Obj = 0.0
  var configFile = new java.io.File("gurobiOption.txt")
  var env = new GRBEnv()
  var model = new GRBModel(env)

  def startModelBuilding(nbRows: Int, nbCols: Int) {
    println("model gurobi:" + nbRows + "x" + nbCols)

    if (configFile.exists()) {
      model.getEnv.readParams(configFile.getAbsolutePath)
    }
    this.nbRows = nbRows
    this.nbCols = nbCols
    val cols = (1 to nbCols).toArray
    var x = model.addVars(cols map (i => 0.0), cols map (i => GRB.INFINITY),
      cols map (i => 0.0), cols map (i => GRB.CONTINUOUS), cols map ("x" + _))
    model.update()
  }

  def endModelBuilding() {
    closed = true
  }

  def setVarName(colId: Int, name: String) {
    model.getVar(colId).set(GRB.StringAttr.VarName, name)
  }
  
  def addConstraint(coef: Array[Double], col: Array[Int], rhs: Double, sign: String, name: String) {
    nbRows += 1
    var ntot = new GRBLinExpr()
    ntot.addTerms(coef, col map (model.getVar(_)))
    sign match {
      case "<=" =>
        model.addConstr(ntot, GRB.LESS_EQUAL, rhs, name)
      case ">=" =>
        model.addConstr(ntot, GRB.GREATER_EQUAL, rhs, name)
      case "==" =>
        model.addConstr(ntot, GRB.EQUAL, rhs, name)
    }
  }
  
  def addConstraintGreaterEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, ">=", name)
  }

  def addConstraintLessEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, "<=", name)
  }
  def addConstraintEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, "==", name)
  }

  override def addConstraintSOS1(col: Array[Int], coef: Array[Double] = null,  name: String) {
    nbRows += 1
    model.addSOS(col.map(model.getVar(_)), coef, GRB.SOS_TYPE1)
  }
  
  def addObjective(coef: Array[Double], col: Array[Int], minMode: Boolean = true) {

    val ntot = toGRBLinExpr(coef, col, model.getVars)
    model.setObjective(ntot, if (minMode) 1 else -1)
    model.update()
  }

  def addColumn(obj: Double, row: Array[Int], coef: Array[Double]) {
    if (!closed) {
      println("cannot add a column in a non closed solver")
    } else {
      nbCols += 1
      val col = new GRBColumn()
      col.addTerms(coef, row map (model.getConstr(_)))
      val v = model.addVar(0.0, GRB.INFINITY, obj, GRB.CONTINUOUS, col, "x" + nbCols)
      model.update()
    }
  }

  def getLowerBound(colId: Int): Double = {
    model.getVar(colId).get(GRB.DoubleAttr.LB)
  }

  def getUpperBound(colId: Int): Double = {
    model.getVar(colId).get(GRB.DoubleAttr.UB)

  }

  def updateLowerBound(colId: Int, lb: Double) {
    model.getVar(colId).set(GRB.DoubleAttr.LB, lb)
  }

  def updateUpperBound(colId: Int, ub: Double) {
    model.getVar(colId).set(GRB.DoubleAttr.UB, ub)

  }

  def solveModel(): LPStatus.Value = {
    model.update()
    // model.write("gurobi.lp")
    model.optimize()
    var optimstatus = model.get(GRB.IntAttr.Status)
    if (optimstatus == GRB.INF_OR_UNBD) {
      model.getEnv().set(GRB.IntParam.Presolve, 0)
      model.optimize()
      optimstatus = model.get(GRB.IntAttr.Status)
      LPStatus.UNBOUNDED
    } else if (optimstatus == GRB.OPTIMAL) {
      sol = Array.tabulate(nbCols)(col => model.getVar(col).get(GRB.DoubleAttr.X))
      Obj = model.get(GRB.DoubleAttr.ObjVal)
      LPStatus.OPTIMAL
    } else if (optimstatus == GRB.INFEASIBLE) {
      println("Model is infeasible")

      // compute and write out IIS
      model.computeIIS()
      LPStatus.INFEASIBLE
    } else if (optimstatus == GRB.UNBOUNDED) {
      println("Model is unbounded")
      LPStatus.UNBOUNDED
    } else {
      sol = Array.tabulate(nbCols)(col => model.getVar(col).get(GRB.DoubleAttr.X))
      println("Optimization was stopped with status = " + optimstatus)
      LPStatus.SUBOPTIMAL
    }
  }

  def getValue(colId: Int): Double = {
    sol(colId)
  }

  def getObjectiveValue(): Double = {
    Obj
  }

  def setInteger(colId: Int) {
    model.getVar(colId).set(GRB.CharAttr.VType, GRB.INTEGER)
  }
  
  def setBinary(colId: Int) {
    model.getVar(colId).set(GRB.CharAttr.VType, GRB.BINARY)
  }
  
  override def setTimeout(t: Int) {
    require(0 <= t)
    model.getEnv().set(GRB.DoubleParam.TimeLimit, t.toDouble)
  }
  
  override def setName(name: String) {
    model.set(GRB.StringAttr.ModelName, name)
  }

  def setFloat(colId: Int) {
    model.getVar(colId).set(GRB.CharAttr.VType, GRB.CONTINUOUS)
  }

  def setBounds(colId: Int, low: Double, up: Double) {
    var grbvar = model.getVar(colId)
    grbvar.set(GRB.DoubleAttr.LB, low)
    grbvar.set(GRB.DoubleAttr.UB, up)
  }

  def setUnboundUpperBound(colId: Int) {
    model.getVar(colId).set(GRB.DoubleAttr.UB, GRB.INFINITY)
  }

  def setUnboundLowerBound(colId: Int) {
    model.getVar(colId).set(GRB.DoubleAttr.LB, -GRB.INFINITY)
  }

  def getReducedCost(colId: Int): Double = {
    model.getVar(colId).get(GRB.DoubleAttr.RC)
  }

  def getDual(rowId: Int): Double = {
    model.getConstr(rowId).get(GRB.DoubleAttr.Pi)
  }

  def deleteConstraint(rowId: Int) {
    model.remove(model.getConstr(rowId))
  }

  def addVariable() {
    nbCols += 1
    var x = model.addVar(0.0, GRB.INFINITY, 0.0, GRB.CONTINUOUS, "x" + nbCols)
    model.update()
  }

  def deleteVariable(colId: Int) {
    model.remove(model.getVar(colId))
  }
  
  /** release the memory associated to the model and the environment as well as the gurobi license*/
  def release() {
    model.dispose
    env.release
    env.dispose
  }
  
  /** Gurobi's export file handling is a little different. The format is defined by the fileName
  * passed to model.write for the LP format it's .lp and for MPS it's .mps
  */ 
  def exportModel(fileName: String, format: LPExportFormat.Value) {
    format match {
        case LPExportFormat.MPS => model.write(fileName + ".mps")
        case LPExportFormat.LP => model.write(fileName + ".lp")
        case _ => model.write(fileName) // User specified file format
    }
  }

  override def addAllConstraints(cons: scala.collection.Map[Int, LPConstraint]) = {

    val vars = model.getVars
    val allCons = cons.values.toArray.sortWith(_.index < _.index)

    model.addConstrs(
      allCons map (c => toGRBLinExpr(c.coef, c.varIds, vars)),
      allCons map (c => toSenses(c.cstr.consType)),
      allCons map (c => c.rhs),
      allCons map (c => c.name))
    model.update()
    println("Added " + allCons.size + " constraints.")
  }

  def setVarProperties(vars: Array[AbstractLPFloatVar]) = {
    val varsGrb = model.getVars
    val orderedVars: Array[GRBVar] = vars map (x => varsGrb(x.index))
    val ub = vars map (_.ub)
    val lb = vars map (_.lb)

    model.set(GRB.DoubleAttr.UB, orderedVars, ub)
    model.set(GRB.DoubleAttr.LB, orderedVars, lb)

    // surprisingly this does not work 
    // model.set(GRB.CharAttr.VType, orderedVars, vars map (x => if(x.isInteger)'I' else 'C'))
    // or this   model.set(GRB.CharAttr.VType, orderedVars, vars map (x => if(x.isInteger)GRB.INTEGER else GRB.CONTINUOUS))
    for (v <- vars filter (_.isInteger)) setInteger(v.index)
    model.update()
  }
  private def toGRBLinExpr(coef: Array[Double], col: Array[Int], vars: Array[GRBVar]): GRBLinExpr = {
    var ntot = new GRBLinExpr()
    ntot.addTerms(coef, col map (vars(_)))
    ntot
  }
  
  private def toSenses(sign: ConstraintType.Value) = {
    sign match {
      case ConstraintType.LQ => GRB.LESS_EQUAL
      case ConstraintType.GQ => GRB.GREATER_EQUAL
      case ConstraintType.EQ => GRB.EQUAL
    }
  }

  def updateRhs(rowId: Int, rhs: Double): Unit = model.getConstr(rowId).set(GRB.DoubleAttr.RHS, rhs)

  def updateCoef(rowId: Int, colId: Int, coeff: Double): Unit = {
    val cons = model.getConstr(rowId)
    val variable = model.getVar(colId)
    
    model.chgCoeff(cons, variable, coeff)
  }

}
