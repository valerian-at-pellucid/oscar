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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by De Landtsheer Renaud
  ******************************************************************************/

package oscar.cbls.routing.model

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSIntVar.int2IntVar
import oscar.cbls.invariants.lib.numeric.{Sum, SumElements}
import oscar.cbls.invariants.lib.minmax.Min2
import oscar.cbls.modeling.Algebra._

/**
 * Maintains a integer weight on each node. This is used to formulate additional model items.
 * @author renaud.delandtsheer@cetic.be
 * THIS IS EXPERIMENTAL
 */
abstract trait NodeWeighting{

  def vrp:VRP
  def name:String

  /**
   * the data structure array which maintains weights.
   */
  val nodeWeight : Array[CBLSIntVar] = Array.tabulate(vrp.N)(i => CBLSIntVar(vrp.m, Int.MinValue, Int.MaxValue, 0,
    name + "_" + i))
}

/** Maintains a cost associated to each vehicle, as the sum of the nodeWeight of each node reached by the vehicle.
  * The initial depot is taken into account in the sum, and it is counted exactly once.
  * @author renaud.delandtsheer@cetic.be
  */
class CommutativeSummedCapacity(val vrp:VRP with NodesOfVehicle, val name:String = "SummedCapacity") extends NodeWeighting{
  val vehicleSum = Array.tabulate(vrp.V)(v => Sum(nodeWeight,vrp.NodesOfVehicle(v)).toIntVar)
}

/** Maintains a capacity that spans on the route of a vehicle.
  * Each node has an incomingCapacity, and an outGoingCapacity.
  * The outgoing capacity is to be set through invariant (but not for depot nodes)
  * This class ensures that the capacityIn of a node is set to the capacityOut of its predecessor;
  *
  * @param vrp the routing model
  * @author renaud.delandtsheer@cetic.be
  */
class AccumulativeCapacity(vrp:VRP with Predecessors, val name:String="AccumulativeCapacity"){

  val capacityOut:Array[CBLSIntVar] = Array.tabulate(vrp.N+1)(n => CBLSIntVar(vrp.m,name = if(n == vrp.N) "capacityOutUnrouted" else name + "_Out_" + n))
  val capacityIn:Array[CBLSIntVar] = Array.tabulate(vrp.N)(n => if (n < vrp.V) 0 else capacityOut.element(vrp.preds(n)))
}

/**
  * This class proposes a standard model of capacity:
  * - there is a maxValueAtNode CBLSIntVar for each node.
  * - the capacityOUt of a node is min(capacityIn,maxValueAtNode)+nodeWeight
  *
  * @param vrp the routing model
  * @author renaud.delandtsheer@cetic.be
  * THIS IS EXPERIMENTAL
  */
class AccumulativeMinSumCapacity(val vrp:VRP with Predecessors, override val name:String="AccumulativeMinSumCapacity")
  extends AccumulativeCapacity(vrp,name) with NodeWeighting{
  val maxValueAtNode = Array.tabulate(vrp.N)(n => CBLSIntVar(vrp.m,name = "maxValueAtNode_" + n))
  for(i <- vrp.nodes){
    capacityOut(i) <== Min2(capacityIn(i), maxValueAtNode(i)) + nodeWeight(i)
  }
}

