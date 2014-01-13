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

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{Store, InvariantHelper, Invariant, IntVar}
import oscar.cbls.invariants.core.propagation.Checker

/**
 * Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i]+offset == j}
 * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
 *
 * it is expected that the values are always >= 0
 */
case class DenseCount(values: Array[IntVar], counts: Array[IntVar], offset:Int = 0) extends Invariant {

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)

  for (count <- counts) { count := 0 }

  for (v <- values.indices) {
    counts(values(v).value+offset) :+= 1
  }

  finishInitialization()

  for (c <- counts) { c.setDefiningInvariant(this) }

  @inline
  override def notifyIntChanged(v: IntVar, index: Int, OldVal: Int, NewVal: Int) {
    assert(values(index) == v)
    counts(OldVal + offset) :-= 1
    counts(NewVal + offset) :+= 1
  }

  override def checkInternals(c: Checker) {
    /**
     * Maintains a count of the indexes of array:
     * count(j) = #{i in index of values | values[i] == j}
     * This is considered as a dense count because counts is
     * an array and must cover all the possibles values of the values
     * in the array ''values''
     */
    val myCounts = Array.fill[Int](counts.length)(0)
    for (i <- values.indices) {
      val v = values(i).getValue(false)
      myCounts(v+offset) = myCounts(v+offset) + 1
    }

    for (j <- counts.indices) {
      c.check(counts(j+offset).getValue(false) == myCounts(j+offset),
        Some("counts(" + j + "+offset).getValue(false) (" + counts(j+offset).getValue(false)
          + ") == myCounts(" + j + "+offset) (" + myCounts(j+offset) + ")"))
    }
  }
}

object DenseCount{
  def makeDenseCount(vars: Array[IntVar]):DenseCount = {
    val ((minMin,maxMax)) = InvariantHelper.getMinMaxBounds(vars)
    val mbValues = maxMax - minMin + 1
    val m:Store = InvariantHelper.findModel(vars)
    val nbVars = vars.length
    val counts = Array.tabulate(mbValues)(i => IntVar(m,0 to nbVars,0,"count_" + (i-minMin)))
    DenseCount(vars,counts,-minMin)
  }
}
