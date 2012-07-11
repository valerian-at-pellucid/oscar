/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Lecture scheduling problem in Oscar.

  Biggs: Discrete Mathematics (2nd ed), page 187.
  """
  Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
  Among the the potential audience there are people who wish to hear both

  - v1 and v2
  - v1 and v4
  - v3 and v5
  - v2 and v6
  - v4 and v5
  - v5 and v6
  - v1 and v6
   *
  How many hours are necessary in order that the lectures can be given
  without clashes?
  """

  Note: This can be seen as a coloring problem.


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Lectures extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    //
    // The schedule requirements:
    // lecture a cannot be held at the same time as b
    // Note: 1-based (compensated in the constraints).
    val g = Array(
                  Array(1, 2),
                  Array(1, 4),
                  Array(3, 5),
                  Array(2, 6),
                  Array(4, 5),
                  Array(5, 6),
                  Array(1, 6))
      
    // number of lectures (nodes)
    val n = 6

    // number of edges
    val edges = g.length

    //
    // variables
    // 

    val v = Array.fill(n)(CPVarInt(cp, 0 to n-1))

    // Maximum color (hour) to minimize.
    // Note: since C# is 0-based, the
    // number of colors is max_c+1.
    val max_c = maximum(v)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(max_c) subjectTo {


      // Ensure that there are no clashes
      // also, adjust to 0-base.
      for(i <- 0 until edges) {
        cp.add(v(g(i)(0)-1) != v(g(i)(1)-1));
      }

      // Symmetry breaking:
      // - v0 has the color 0,
      // - v1 has either color 0 or 1
      cp.add(v(0) == 0);
      cp.add(v(1) <= 1);

    } exploration {
       
      cp.binary(v)
      // cp.binaryFirstFail(v)
      // cp.binaryMaxDegree(v)

      println("\nSolution:")

      println("max hour: " + max_c)
      println("v: " + v.mkString(""))

      for(i <- 0 until n) {
        println("Lecture " + i + " at " + v(i) + "h")
      }

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
