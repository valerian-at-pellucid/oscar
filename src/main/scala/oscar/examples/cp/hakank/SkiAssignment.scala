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


/**

  Ski assignment in Oscar
 
  From  
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008 – Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.

  """

 
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object SkiAssignment extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val num_skis   = 6
    val num_skiers = 5;

    val ski_heights   = Array(1, 2, 5, 7, 13, 21)
    val skier_heights = Array(3, 4, 7, 11, 18)
      
    // variables
    val x = Array.fill(num_skiers)(CPVarInt(cp, 1 to num_skis))
    // sum of differences between height of assigned skis to the skiers
    val z = sum(Array.tabulate(num_skiers)(i=> (element(ski_heights, x(i)) - skier_heights(i)).abs()))

    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      cp.add(alldifferent(x), Strong)

     } exploration {
       
       cp.binaryFirstFail(x)

       for(s <- 0 until num_skiers) {
         println("skier " + s + " ski: " + x(s))
       }
       println()

       numSols += 1
       
     }

     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
