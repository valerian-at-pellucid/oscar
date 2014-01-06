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
package oscar.examples.cp

import oscar.cp.modeling._
import oscar.visual._
import java.awt.Color
import oscar.visual.shapes.VisualText
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle
import oscar.visual.shapes.VisualCircle
import oscar.cp.core.CPVarInt

/**
 * Little Problem given by my n-Side colleague Audrey Timmermans:
 * Based on a little game I used to play in high school when I was getting bored in the classroom...
 * Draw a ten cells by ten cells board.
 * The purpose is to fill in all cells with numbers from 1 to 100.
 * You start by writing 1 in whatever cell.
 * From there on, you need to write the 2 by moving around in one of the following ways:
 * - Move by 3 cells horizontally or vertically
 * - Or move by 2 cells diagonally
 * Then, starting from the 2, you need to write the 3 using the same permitted moves, and so on...
 * @author Pierre Schaus pschaus@gmail.com
 */
object AudreyProblem extends CPModel with App {

  def reachables(i: Int): Set[Int] = {
    val l = i / 10
    val c = i % 10
    val neighbors = Set((l - 3, c), (l + 3, c), (l, c + 3), (l, c - 3), (l + 2, c + 2), (l + 2, c - 2), (l - 2, c + 2), (l - 2, c - 2)).filter { case (a, b) => a < 10 && a >= 0 && b < 10 && b >= 0 }
    neighbors.map { case (a, b) => a * 10 + b }
  }

  // Variables
  val x = Array.tabulate(100)(i => CPVarInt(reachables(i)))
  
  onSolution { 
    // Print solution
    for (i <- 0 until 100 by 10) {
      for (j <- 0 until 10) {
        val s = x(i+j).value.toString
        if (s.size == 1) print(" " + s + " ") else print(s + " ")
      }
      println
    }
  }
 
  // Constraints
  add(circuit(x))
  
  // Search 
  search { binaryFirstFail(x) }
  
  // Execution
  start(nSolutions = 1)
}