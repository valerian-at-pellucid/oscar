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
import oscar.cbls.invariants.lib.logic.{Cluster, IntElement}
import oscar.cbls.invariants.lib.numeric.{Sum, SumElements}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.computation._
;

//TODO: test this
/**This is the standard bin packing constraint
 * WARNING: not tested!
 * @param items the items, designing the bins they are placed into
 * @param itemsizes the size of the items
 * @param binsizes the max size of the available bins
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class MultiKnapsack(items: Array[IntVar], itemsizes: Array[IntVar], binsizes:Array[IntVar])
  extends Constraint {

  registerConstrainedVariablesAll(items)
  registerConstrainedVariablesAll(itemsizes)
  registerConstrainedVariablesAll(binsizes)

  finishInitialization()

  val bincontents:Array[IntSetVar] = Cluster.MakeDense(items).clusters
  val binfilling:Array[IntVar] = bincontents.map(bincontent => SumElements(itemsizes,bincontent).toIntVar)

  val binviolations:Array[IntVar] = (
    for (binid <- binsizes.indices)
    yield (binfilling(binid) le binsizes(binid)).violation).toArray

  val itemviolations:Array[IntVar] = items.map(itemval => IntElement(itemval,binviolations).toIntVar)

  val Violation:IntVar = Sum(binviolations).toIntVar

  val Violations:SortedMap[IntVar,IntVar] = {
    var acc = SortedMap.empty[IntVar,IntVar]
    for(itemid <- items.indices){
      acc += ((items(itemid),itemviolations(itemid)))
      acc += ((itemsizes(itemid),itemviolations(itemid)))
    }
    for(binid <- binsizes.indices){
      acc += ((binsizes(binid),binviolations(binid)))
    }
    acc
  }

  /**The violation of the constraint is the sum on all excess in all bins.
   */
  override def violation = Violation

  /**The violation of an item is the excess of the bin it is located into,
   * The violation of a bin is the excess in the bin
   */
  override def violation(v: Variable): IntVar = {
    val tmp:IntVar = Violations.getOrElse(v.asInstanceOf[IntVar],null)
    assert(tmp != null)
    tmp
  }
}