/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
 ******************************************************************************/
package oscar.search


import oscar.reversible.ReversibleSearchNode
import scala.collection.mutable.Stack
import scala.util.continuations._




/**
 * Non deterministic search controller as described in the wonderful paper:
 * "Non Deterministic Control for Hybrid Search" by Pascal Van Hentenryck and Laurent Michel.
 * In their paper they introduce the nicest possible way to implement a search for CP (in Comet).
 * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
 */
class DFSSearchController(node: ReversibleSearchNode) extends SearchController(node) {
  
  val stack: Stack[MyContinuation] = new Stack()
  
  def addChoice(e: MyContinuation) { 
	node.pushState()
    stack.push(e)
  } 
  
  
  override def reset() {
    super.reset()
    stack.clear()
  }
  
  
  def explore() {
    while(!stack.isEmpty && !limitReached && !exit) {
      node.pop()
      fail()
      stack.pop.call()
    }
  }

}

