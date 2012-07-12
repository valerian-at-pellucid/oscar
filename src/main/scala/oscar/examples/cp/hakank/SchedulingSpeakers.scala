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
 * Scheduling speakers problem in Oscar
 *
 * From Rina Dechter, Constraint Processing, page 72
 * Scheduling of 6 speakers in 6 slots.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SchedulingSpeakers extends CPModel {


  //
  // ensure that x is a member of array y
  //
  def isMember(cp: CPSolver, x: CPVarInt, y: Array[Int]) {
    
      cp.add(sum(for{j <- y} yield (x === j)) == 1)

  }


   def main(args: Array[String]) {

      val cp = CPSolver()

      //
      // data
      // 
      val num_speakers = 6
      val num_slots    = 6

      // Slots available to speakers
      val available = Array(
                            //                    Reasoning:
                            Array(3,4,5,6),    // 2) the only one with 6 after speaker F -> 1
                            Array(3,4),        // 5) 3 or 4
                            Array(2,3,4,5),    // 3) only with 5 after F -> 1 and A -> 6
                            Array(2,3,4),      // 4) only with 2 after C -> 5 and F -> 1
                            Array(3,4),        // 5) 3 or 4
                            Array(1,2,3,4,5,6) // 1) the only with 1
                            )


      //
      // decision variables
      // 

      val speakers = Array.fill(num_speakers)(CPVarInt(cp, 1 to num_slots ))

      var numSols = 0
      cp.solveAll() subjectTo {

        cp.add(alldifferent(speakers))

        // Assign speakers at available slots
        for(i <- 0 until num_speakers) {
          isMember(cp, speakers(i), available(i))
        }


      } exploration {

        cp.binary(speakers)

        println(speakers.mkString(""))

        numSols += 1
      }

      println("\nIt was " + numSols + " solutions.")	  
      cp.printStats()

  }

}
