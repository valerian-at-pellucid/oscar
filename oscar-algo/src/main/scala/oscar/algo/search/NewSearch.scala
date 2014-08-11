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

package oscar.algo.search

import oscar.algo.reversible._

/**
 *  New DFS search
 *  
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class NewSearch(node: SearchNode) {

  import oscar.algo.ArrayStack // custom array-based stack
  private val alternativesStack = new ArrayStack[Iterator[Alternative]](100)

  // Number of backtracks of the previous search
  private var nbBkts: Int = 0

  // Number of solutions of the previous search
  private var nbSols: Int = 0

  // Number of nodes explored in the previous search
  private var nbNodes: Int = 0

  // Actions to execute in case of solution node
  private var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private var failureActions = List.empty[() => Unit]

  /** Returns the number of backtracks in the previous search */
  final def nBacktracks: Int = nbBkts

  /** Returns the number of solutions found in the previous search */
  final def nSolutions: Int = nbSols

  /** Returns the number nodes explored in the previous search */
  final def nNodes: Int = nbNodes

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  @inline private def expand(branching: Branching): Boolean = {
    val alternatives = branching.alternatives
    if (alternatives.isEmpty) false
    else {
      alternativesStack.push(alternatives.iterator)
      true
    }
  }

  final def start(branching: Branching, stopCondition: => Boolean): Unit = start(branching, (s:NewSearch) => stopCondition)
  
  final def start(branching: Branching, stopCondition: NewSearch => Boolean = _ => false): Unit = {

    // Initializes the search
    node.resetStats() // resets trailing time too
    alternativesStack.clear()
    nbSols = 0
    nbBkts = 0
    nbNodes = 0

    node.pushState()

    // Expand the root node
    if (!node.isFailed) {
      node.pushState()
      val isExpandable = expand(branching)
      if (!isExpandable) {
        node.solFound()
        solutionActions.foreach(_())
        nbSols += 1
      }
    }

    while (!alternativesStack.isEmpty && !stopCondition(this)) {

      nbNodes += 1

      val alternatives = alternativesStack.top
      val alternative = alternatives.next()
      
      val isLast = !alternatives.hasNext
      
      if (!isLast) node.pushState()
      else alternativesStack.pop() // no more alternative in the sequence

      alternative() // apply the alternative

      if (!node.isFailed()) {
        val isExpandable = expand(branching)
        if (!isExpandable) {
          node.solFound()
          solutionActions.foreach(_())
          nbSols += 1
          nbBkts += 1
          node.pop()
        }
      } else {
        failureActions.foreach(_())
        nbBkts += 1
        node.pop
      }
    }
    
    // Pop the remaining nodes 
    var i = alternativesStack.size
    while (i != 0) {
      node.pop
      i -= 1
    }
    node.pop()
  }
}