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
package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.algebra._

/* 
 * The knapsack problem is a well-known problem in combinatorial optimization: 
 * Given a set of items, each with a weight and an utility, determine the count of each item 
 * to include in a collection so that the total weight is less than or equal to a given limit
 * and the total utility is as large as possible.
 * @author gme
 */
object KnapSack extends MIPModel with App {

  case class O(val weight: Int, val utility: Int, val x: MIPFloatVar)
  val weights = Array(100, 50, 45, 20, 10, 5)
  val utility = Array(40, 35, 18, 4, 10, 2)

  val objects = Array.tabulate(weights.size)(i => O(weights(i), utility(i), MIPIntVar("x" + i, 0 to 1)))

  val capacity = 100

  // maximize total utility
  maximize(sum(objects)(o => o.x * o.utility))

  // given the limited capacity of the pack
  add(sum(objects)(o => o.x * o.weight) <= capacity)
  start()
  val selected = objects.filter(o => o.x.value.get >= .9)
  var totalWeight = selected.map(o => o.weight).sum

  println("Status: " + status)
  println("Total Utility: " + objectiveValue)
  println("Total Weight: " + totalWeight)

  release()

}
