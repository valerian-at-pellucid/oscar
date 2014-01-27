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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.{InvariantHelper, Variable, CBLSIntVar}
import oscar.cbls.invariants.lib.logic.{DenseCount, IntElement, IntVar2IntVarFun}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.numeric.Sum

/**Implements the AtMost constraint on IntVar.
  * There is a set of bounds, defined in the parameter bound as pair (value,bound).
  * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
  *
  * @param variables the variables that should be bounded
  * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
  * @author renaud.delandtsheer@cetic.be
  */
case class AtMost(variables:Iterable[CBLSIntVar], bounds:SortedMap[Int, Int]) extends Constraint {
  assert(variables.size < Int.MaxValue)

  model = InvariantHelper.findModel(variables)
  registerConstrainedVariables(variables)
  finishInitialization()

  private val countInvariant = DenseCount.makeDenseCount(variables.toArray)
  private val offset:Int = countInvariant.offset
  private val valueCount = countInvariant.counts //v => #occurrence of v+offset in variables

  private val noViolation:CBLSIntVar = 0
  private val violationByVal=Array.tabulate(valueCount.length)(_ => noViolation)

  for((value,bound) <- bounds){
    violationByVal(value) = Max2(noViolation,valueCount(value) - bound).toIntVar
  }

  //the violation of each input variable
  private val Violations:SortedMap[CBLSIntVar,CBLSIntVar] = variables.foldLeft(SortedMap.empty[CBLSIntVar,CBLSIntVar])((acc,intvar)
  => {
    val newVar = new CBLSIntVar(model,(0 to 1),1,"Violation_AtMost_"+intvar.name)
    newVar <== violationByVal.element(intvar + offset)
    acc + ((intvar,newVar))
  })

  private val Violation:CBLSIntVar = new CBLSIntVar(model,(0 to Int.MaxValue), 0,"ViolationsOfAtMost")
  Violation <== Sum(bounds.keys.map(bound => violationByVal(bound)))

  /**The violation of the constraint is the sum on all bound of the number of variable that are in excess.
    * the number of variable in excess is the max between zero and
    * (the number of variable that have the value of the bound minus the bound).
    */
  override def violation = Violation

  /**The violation of a variable is zero if its value is not the one of a bound.
    * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
    */
  override def violation(v: Variable):CBLSIntVar = {
    Violations(v.asInstanceOf[CBLSIntVar])
  }

  override def checkInternals(c: Checker) {
    var checkBounds:SortedMap[Int, Int] = SortedMap.empty
    for(i <- bounds.keys) checkBounds += ((i,0))
    for (v <- variables) if (checkBounds.isDefinedAt(v.value)) checkBounds += ((v.value,checkBounds(v.value) +1))

    for (v <- variables){
      /*The violation of a variable is zero if its value is not the one of a bound.
        * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
        */
      val violationOfV = violation(v)
      val expectedViolation =
        if (checkBounds.isDefinedAt(v.value)) 0.max(checkBounds(v.value) - bounds(v.value))
        else 0
      c.check(violationOfV.value == expectedViolation, Some("" + violationOfV + "== expectedViolation" + expectedViolation))
    }

    /*The violation of the constraint is the sum on all bound of the number of variable that are in excess.
      * the number of variable in excess is the max between zero and
      * (the number of variable that have the value of the bound minus the bound).*/
    var summedViolation = 0;
    for(i <- bounds.keys){
      if (checkBounds(i) > bounds(i)) summedViolation += (checkBounds(i) - bounds(i))
    }
    c.check(summedViolation == violation.value, Some("summedViolation ("+summedViolation+") == violation.value ("+violation.value+")"))
  }
}

