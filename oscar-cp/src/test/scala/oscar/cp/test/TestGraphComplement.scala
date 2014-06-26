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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.core.CPOutcome._

import oscar.cp.modeling._

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestGraphComplement extends FunSuite with ShouldMatchers  {
  
  test("Test 1 : Test constraint initial propagation") {
    // we will test with two graph that first propagation does correct pruning
    val cp = CPSolver()
    val nnodes1 : Int = 5
    val edges1 = List((0,1),(0,2),(1,2),(2,3),(1,4))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(0,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    // 1) add some mandatory nodes/edges
    for (x <- 0 to 2) {
      cp.post(g1.addNode(x)) should be (Suspend)
      cp.post(g2.addNode(x)) should be (Suspend)
    }
    cp.post(g2.addNode(3)) should be (Suspend)
    cp.post(g1.addEdge(0,1)) should be (Suspend)
    
    // 2) check that all is correct : 
    // check nodes
    g1.possibleNodes should be (List(0,1,2,3,4))
    g2.possibleNodes should be (List(0,1,2,3))
    g1.requiredNodes should be (List(0,1,2))
    g2.requiredNodes should be (List(0,1,2,3))
    
    // check edges
    g1.requiredEdges(0).sorted    should be (List(0))
    g2.requiredEdges(0).sorted    should be (List())
    g1.requiredEdges(1).sorted    should be (List(0))
    g2.requiredEdges(1).sorted    should be (List())
    for (x <- 2 to 3) {
      g1.requiredEdges(x).sorted  should be (List())
      g2.requiredEdges(x).sorted  should be (List())
    }
    g1.requiredEdges(4).sorted    should be (List())
    g1.possibleEdges(0).sorted 	  should be (List(0,1))
    g1.possibleEdges(1).sorted 	  should be (List(0,2,4))
    g1.possibleEdges(2).sorted 	  should be (List(1,2,3))
    g1.possibleEdges(3).sorted 	  should be (List(3))
    g1.possibleEdges(4).sorted 	  should be (List(4))
    g2.possibleEdges(0).sorted 	  should be (List(0,1))
    g2.possibleEdges(1).sorted 	  should be (List(0))
    g2.possibleEdges(2).sorted 	  should be (List(1))
    g2.possibleEdges(3).sorted 	  should be (List())
    
    
    // 3) add constraint
    cp.post(new GraphComplement(g1,g2)) should be (Suspend)
    
    // 4) check that all changes are correct acc. to definition
    // check nodes
    g1.possibleNodes should be (List(0,1,2,3))
    g2.possibleNodes should be (List(0,1,2,3))
    g1.requiredNodes should be (List(0,1,2,3))
    g2.requiredNodes should be (List(0,1,2,3))
    
    // check edges
    g1.requiredEdges(0).sorted    should be (List(0))
    g2.requiredEdges(0).sorted    should be (List())
    g1.requiredEdges(1).sorted    should be (List(0,2))
    g2.requiredEdges(1).sorted    should be (List())
    g1.requiredEdges(2).sorted    should be (List(2,3))
    g2.requiredEdges(2).sorted    should be (List())
    g1.requiredEdges(3).sorted    should be (List(3))
    g2.requiredEdges(3).sorted    should be (List())

    g1.possibleEdges(0).sorted 	  should be (List(0,1))
    g1.possibleEdges(1).sorted 	  should be (List(0,2))
    g1.possibleEdges(2).sorted 	  should be (List(1,2,3))
    g1.possibleEdges(3).sorted 	  should be (List(3))
    g2.possibleEdges(0).sorted 	  should be (List(1))
    g2.possibleEdges(1).sorted 	  should be (List())
    g2.possibleEdges(2).sorted 	  should be (List(1))
    g2.possibleEdges(3).sorted 	  should be (List())
  }
  
  test("Test 2 : Add required edge") {
    // we start from graph as described in test 1 to which we will add an edge
    val cp = CPSolver()
    val nnodes1 : Int = 5
    val edges1 = List((0,1),(0,2),(1,2),(2,3),(1,4))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(0,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    for (x <- 0 to 2) {
      cp.post(g1.addNode(x)) should be (Suspend)
      cp.post(g2.addNode(x)) should be (Suspend)
    }
    cp.post(g2.addNode(3)) should be (Suspend)
    cp.post(g1.addEdge(0,1)) should be (Suspend)
    cp.post(new GraphComplement(g1,g2)) should be (Suspend)
    
    // add required edge in g1
    cp.post(g1.addEdge(0,2)) should be (Suspend)
    // -> should remove it from g2 to be consistent during propagation 
    
    // check edges
    g1.requiredEdges(0).sorted    should be (List(0,1))
    g2.requiredEdges(0).sorted    should be (List())
    g1.requiredEdges(1).sorted    should be (List(0,2))
    g2.requiredEdges(1).sorted    should be (List())
    g1.requiredEdges(2).sorted    should be (List(1,2,3))
    g2.requiredEdges(2).sorted    should be (List())
    g1.requiredEdges(3).sorted    should be (List(3))
    g2.requiredEdges(3).sorted    should be (List())

    g1.possibleEdges(0).sorted 	  should be (List(0,1))
    g1.possibleEdges(1).sorted 	  should be (List(0,2))
    g1.possibleEdges(2).sorted 	  should be (List(1,2,3))
    g1.possibleEdges(3).sorted 	  should be (List(3))
    // should no longer have edges 
    g2.possibleEdges(0).sorted 	  should be (List())
    g2.possibleEdges(1).sorted 	  should be (List())
    g2.possibleEdges(2).sorted 	  should be (List())
    g2.possibleEdges(3).sorted 	  should be (List())
  }
  
  test("Test 3 : Remove possible edge") {
	// we start from graph as described in test 1 to which we will remove an edge
    val cp = CPSolver()
    val nnodes1 : Int = 5
    val edges1 = List((0,1),(0,2),(1,2),(2,3),(1,4))
    val g1 = CPGraphVar(cp, nnodes1, edges1)
    val nnodes2 : Int = 4
    val edges2 = List((0,1),(0,2))
    val g2 = CPGraphVar(cp, nnodes2, edges2)
    
    for (x <- 0 to 2) {
      cp.post(g1.addNode(x)) should be (Suspend)
      cp.post(g2.addNode(x)) should be (Suspend)
    }
    cp.post(g2.addNode(3)) should be (Suspend)
    cp.post(g1.addEdge(0,1)) should be (Suspend)
    cp.post(new GraphComplement(g1,g2)) should be (Suspend)
    
    // remove possible edge in g1
    cp.post(g1.removeEdge(0,2)) should be (Suspend)
    // -> should set it required in g2 to be consistent during propagation 
    
    // check edges : node 1:(0,2) should be required in g2
    g1.requiredEdges(0).sorted    should be (List(0))
    g2.requiredEdges(0).sorted    should be (List(1))
    g1.requiredEdges(1).sorted    should be (List(0,2))
    g2.requiredEdges(1).sorted    should be (List())
    g1.requiredEdges(2).sorted    should be (List(2,3))
    g2.requiredEdges(2).sorted    should be (List(1))
    g1.requiredEdges(3).sorted    should be (List(3))
    g2.requiredEdges(3).sorted    should be (List())

    g1.possibleEdges(0).sorted 	  should be (List(0))
    g1.possibleEdges(1).sorted 	  should be (List(0,2))
    g1.possibleEdges(2).sorted 	  should be (List(2,3))
    g1.possibleEdges(3).sorted 	  should be (List(3))
    g2.possibleEdges(0).sorted 	  should be (List(1))
    g2.possibleEdges(1).sorted 	  should be (List())
    g2.possibleEdges(2).sorted 	  should be (List(1))
    g2.possibleEdges(3).sorted 	  should be (List())
  }
      
}