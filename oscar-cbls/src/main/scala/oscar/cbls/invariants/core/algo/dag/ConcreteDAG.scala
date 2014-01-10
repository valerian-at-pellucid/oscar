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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.core.algo.dag

class ConcreteDAGNode(val _UniqueID:Int) extends DAGNode{

  UniqueID = _UniqueID

  var PrecedingNodes: List[DAGNode] = List.empty
  var SucceedingNodes:List[DAGNode] = List.empty

  final def compare(that: DAGNode):Int = {
    assert(this.UniqueID != that.UniqueID || this == that)
    this.UniqueID - that.UniqueID
  }

  override def getDAGPrecedingNodes: Iterable[DAGNode] = PrecedingNodes
  override def getDAGSucceedingNodes: Iterable[DAGNode] = SucceedingNodes

  def setAsPrecedingNodeKnownNotYetPreceding(b:ConcreteDAGNode){
    PrecedingNodes = b :: PrecedingNodes
    b.SucceedingNodes = this ::b.SucceedingNodes
  }

  def setAsSucceedingNodeKnownNotYetSucceeding(b:ConcreteDAGNode){
    b.setAsPrecedingNodeKnownNotYetPreceding(this)
  }
}

class ConcreteDAG(Nodes:Iterable[DAGNode]) extends DAG{
//  var Nodes:List[DAGNode] = List.empty
  def nodes:Iterable[DAGNode] = Nodes
}

