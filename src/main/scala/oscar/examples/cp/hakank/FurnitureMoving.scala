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

  Furniture Moving (scheduling) problem in Oscar.

  Problem from Marriott & Stuckey: 'Programming with constraints', page  112f

  This model includes a decomposition of the cumulative constraint.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object FurnitureMoving extends CPModel {

  // Thanks Pierre Schaus for help with this decomposition of cumulative.
  def myCumulative(cp: CPSolver,
                   s: Array[CPVarInt],
                   d: Array[Int],
                   r: Array[Int],
                   b: CPVarInt) {
    
    val tasks = for{
      i <- 0 to s.length-1
      if (r(i) > 0 && d(i) > 0)
        } yield i

    val times_min = tasks.map(i => s(i).getMin()).min
    val d_max = d.max
    val times_max = tasks.map(i => s(i).getMax() + d_max).min
            
    for(t <- times_min to times_max) {
      cp.add(sum(tasks)(i => ((s(i) <<= t) && (s(i)+d(i) >>= t)) * r(i) ) <= b)
    }

    cp.add(b <= r.sum);
        
  }                                                                                                                

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 4
    val upper_limit = 160

    val durations = Array(30,10,15,15)
    val resources = Array(3,1,3,2)

    //
    // variables
    //
    val starts = Array.fill(n)(CPVarInt(cp, 0 to upper_limit))
    val end_times = Array.fill(n)(CPVarInt(cp, 0 to upper_limit*2))
    val num_persons = CPVarInt(cp, 1 to 100)

    val max_end_time = maximum(end_times) 


    //
    // constraints
    //
    var numSols = 0
    // cp.minimize(max_end_time) subjectTo {
    cp.minimize(num_persons) subjectTo{ 
      

      // ensure that end_times is starts + duration
      for(i <- 0 until n) {
        cp.add(end_times(i) == starts(i) + durations(i))
      }

      // when searching for max_end_time
      // cp.add(num_persons == 3)
  
      // constraints
      myCumulative(cp, starts, durations, resources, num_persons)
  

    } exploration {
       
      cp.binaryFirstFail(starts ++ Array(num_persons))

      println("\nSolution:")

      println("num_persons : " + num_persons)
      println("max_end_time: " + max_end_time)
      println("start       : " + starts.mkString(" "))
      println("durations   : " + durations.mkString(" "))
      println("resources   : " + resources.mkString(" "))
      println("end_times   : " + end_times.mkString(" "))
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
