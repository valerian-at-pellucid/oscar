/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._

/**
 *
 * Seseman problem in Oscar.
 *
 * For a (Swedish) discussion of this problem see
 * "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
 *  http://www.hakank.org/webblogg/archives/001084.html
 * and
 * Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
 * (using ECLiPSe CLP code)
 *
 *
 * n is the length of a border
 * There are (n-2)^2 "holes", i.e.
 * there are n^2 - (n-2)^2 variables to find out.
 *
 * The simplest problem, n = 3 (n x n matrix)
 * which is represented by the following matrix:
 * 
 *   a b c 
 *   d   e 
 *   f g h 
 *
 * Where the following constraints must hold:
 *
 *    a + b + c = border_sum
 *    a + d + f = border_sum
 *    c + e + h = border_sum
 *    f + g + h = border_sum
 *    a + b + c + d + e + f = total_sum
 *
 *  For larger matrices just the borders is summed.
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Seseman extends CPModel {

   def main(args: Array[String]) {

     val cp = CPSolver()

     // data
     val n = 3
     val border_sum = n*n

     // variables
     val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 0 to n*n)))
     val total_sum = CPVarInt(cp, 1 to n*n*n*n)

     // constraints
     var numSols = 0
     cp.solveAll subjectTo {

       // 0's in all the middle cells
       for(i <- 1 until n-1) {
         for(j <- 1 until n-1) {
           cp.add(x(i)(j) == 0)
         }
       }

       // sum the borders
       cp.add(sum(List.tabulate(n)(i => x(i)(0)))   == border_sum)
       cp.add(sum(List.tabulate(n)(i => x(i)(n-1))) == border_sum)
       cp.add(sum(List.tabulate(n)(i => x(0)(i)))   == border_sum)
       cp.add(sum(List.tabulate(n)(i => x(n-1)(i))) == border_sum)
  
       // all borders must be >= 1 (may be changed to 0 or whatever)
       for(i <- 0 until n) {
         for(j <- 0 until n) {
           if ((i == 0) || (j == 0) || (i == n-1) || (j == n-1)) {
             cp.add(x(i)(j) > 0)
           }
         }
       }

       cp.add(total_sum == sum(x.flatten))


     } exploration {
       
       cp.binaryFirstFail(x.flatten)
       for(i <- 0 until n) {
         for(j <- 0 until n) {
           print(x(i)(j) + " ")
         }
         println()
       }
       println()
       numSols += 1
       
     }

     println("\nIt was " + numSols + " solutions.")
     cp.printStats()
   }

}
