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
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._


/**
 *
 * Map coloring in Oscar
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object MapColoring {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val num_colors = 4

    // Belgium, Denmark, France, Germany, Netherlands, Luxembourg
    val connections = Array(Array(0, 0, 1, 1, 1, 1),
                            Array(0, 0, 0, 1, 0, 0),
                            Array(1, 0, 0, 1, 1, 0),
                            Array(1, 1, 1, 0, 1, 1),
                            Array(1, 0, 1, 1, 0, 0),
                            Array(1, 0, 0, 1, 0, 0))

    val num_countries = connections.length
      
    // variables
    val color = Array.fill(num_countries)(CPVarInt(cp, 1 to num_colors))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      for (c1 <- 0 until num_countries; 
           c2 <- 0 until c1 if connections(c1)(c2)==1) {
          cp.add(color(c1) != color(c2))
      }

      // Symmetry breaking: Belgium has color 1
      cp.add(color(0) == 1)


     } exploration {
       
       cp.binaryFirstFail(color)

       println("color:" + color.mkString(""))

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
