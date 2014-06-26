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


package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Defines the constraint Undirected(G1,G2)
 * 	this constraint holds if G2 is the undirected (symmetric) graph obtained by ignoring the direction of the edges in G1 
 */

class GraphUndirected(val g1 : CPGraphVar, val g2: CPGraphVar) extends Constraint(g1.s, "Undirected") {
  
	override def setup(l: CPPropagStrength): CPOutcome = {
	  // add filter when domain changes
	  g1.callPropagateWhenDomainChanges(this)
	  g2.callPropagateWhenDomainChanges(this)
	  propagate()
	}
	
	override def propagate(): CPOutcome = {  	  
	  // we have to check that nodes sets are the same
	  val g1PossNodes = g1.possibleNodes
	  val g1ReqNodes = g1.requiredNodes
	  val g2PossNodes = g2.possibleNodes
	  val g2ReqNodes = g2.requiredNodes
	  for (n <- g1PossNodes; if !g2PossNodes.contains(n))
	      if (g1.removeNodeFromGraph(n) == Failure) return Failure
	  for (n <- g2PossNodes; if !g1PossNodes.contains(n))
	      if (g2.removeNodeFromGraph(n) == Failure) return Failure      
	  for (n <- g1ReqNodes; if !g2ReqNodes.contains(n))
	      if (g2.addNodeToGraph(n) == Failure) return Failure
	  for (n <- g2ReqNodes; if !g1ReqNodes.contains(n))
	      if (g1.addNodeToGraph(n) == Failure) return Failure
	  
	  // Prune G1
	  //	remove from the possible edges of G all edges that are not possible in G2
	  val newG1PossNodes = g1.possibleNodes
	  for (n <- newG1PossNodes){
	    for (e <- g1.possibleEdges(n)){
	      val (src,dest) = g1.edge(e)
	      if ( ! g2.possibleEdges(n).map(g2.edge(_)).contains((src,dest)))
	        if (g1.removeEdgeFromGraph(src,dest) == Failure) return Failure
	    }
	  }
	  // 	include in the required set of G if there is a corresponding edge in G2
	  val newG2ReqNodes = g2.requiredNodes
	  for (n <- newG2ReqNodes){
	    for (e <- g2.requiredEdges(n)){
	      val (src,dest) = g2.edge(e)
	      // if g1.possibleEdges contains (src,dest) which is required in g2, set it required in g1 
	      if (g1.possibleEdges(n).map(g1.edge(_)).contains((src, dest)))
	    	if (g1.addEdgeToGraph(src, dest) == Failure) return Failure
	    }
	  }    
	      
	  // Prune G2
	  // 	include in the required set of G2 every required edge of G1 and its reverse edge
	  val newG1ReqNodes = g1.requiredNodes
	  for (n <- newG1ReqNodes){
	    for (e <- g1.requiredEdges(n)){
	      val (src,dest) = g1.edge(e)
	      if (g2.addEdgeToGraph(src, dest) == Failure) return Failure
	      if (g2.addEdgeToGraph(dest, src) == Failure) return Failure
	    }
	  }
	  //    remove from the possible edges of G2 every edge which is not possible in G1 nor the reverse of it
	  val newG2PossNodes = g2.possibleNodes
	  for (n <- newG2PossNodes){
	    for (e <- g2.possibleEdges(n); if !g2.requiredEdges(n).contains(e)){
	      val (src,dest) = g2.edge(e)
	      val g1PossEdgesMap = g1.possibleEdges(n).map(g1.edge(_))
	      if ( !g1PossEdgesMap.contains((src,dest)) && !g1PossEdgesMap.contains((dest,src)))
	        if (g2.removeEdgeFromGraph(src,dest) == Failure) return Failure
	    }
	  }

	  Suspend
	}	
}