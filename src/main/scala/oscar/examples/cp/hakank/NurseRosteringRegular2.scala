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
import oscar.cp.constraints._
import collection.mutable._
import scala.collection.JavaConversions._

/**
 *
 * Nurse rostering using regular in Oscar.
 *
 * This is a simple nurse rostering model using a DFA and
 * the built-in regular with Automaton.
 *
 * The DFA is from the Nurse Rostering example in
 * page 37 ff in the "MiniZinc tutorial"
 * http://www.g12.cs.mu.oz.au/minizinc/downloads/doc-1.5.1/minizinc-tute.pdf
 * The DFA is at page 39.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object NurseRosteringRegular2 extends CPModel {

  def maxDomNotbound(vars: Iterable[CPVarInt]): Iterable[(CPVarInt, Int)] = {
    val notbound = vars.filterNot(_.isBound)
    if (notbound.nonEmpty) {
      val sizeMax = notbound.map(_.getSize).max
      notbound.zipWithIndex.filter {
        _._1.getSize == sizeMax
      }
    } else {
      Iterable()
    }
  }
 


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    // 
    var num_to_show = 1 // 0: show all solutions
     
    if (args.length > 0) {
      num_to_show = args(0).toInt
    }



    // Note: If you change num_nurses or num_days,
    //       please also change the constraints
    //       on nurse_stat and/or day_stat.
    val num_nurses = 7
    val num_days = 14
      // val num_days = 7



    val nurses = 0 until num_nurses
    val days = 0 until num_days

    // the shifts
    val day_shift   = 0
    val night_shift = 1
    val off_shift   = 2

    // different usages of the shifts
    val shifts_a       = Array(day_shift, night_shift, off_shift)
    val shifts_r       = day_shift to off_shift
    val num_shifts     = shifts_a.length

     // The DFA is from MiniZinc Tutorial, Nurse Rostering example:
     // - one day off every 4 days
     // - no 3 nights in a row.
      
    // state, next state, input (base = 0)
    val transition_tuples = Array(Array(0,1,0),  // state 0: off
                                  Array(0,2,1),  
                                  Array(0,0,2),
                                  Array(1,3,0),  // state 1: dayshift 1
                                  Array(1,3,1),
                                  Array(1,0,2),
                                  Array(2,3,0),  // state 2: night shift 1
                                  Array(2,4,1),
                                  Array(2,0,2),
                                  Array(3,5,0),  // state 3: dayshift 2 / night 1
                                  Array(3,5,1),
                                  Array(3,0,2),
                                  Array(4,5,0),  // state 4: night 2
                                  Array(4,0,2),
                                  Array(5,0,2))  // dayshift 3 / day 1 after night 2

    val days_str = Array("d","n","o") // for presentation

    // the DFA (for regular)
    val initial_state = 0
    val accepting_states: Set[java.lang.Integer] = Set(0,1,2,3,4,5)
    val num_states  = 6
    val num_letters = shifts_a.length

    val automaton = new Automaton(num_states, num_letters, initial_state, accepting_states)
    transition_tuples.foreach(a => 
                              automaton.addTransition(a(0), a(1), a(2))
                              )


    //
    // variables
    //
    val x = Array.fill(num_nurses,num_days)(CPVarInt(cp, shifts_r))
    val stat = Array.fill(num_days,num_shifts)(CPVarInt(cp, nurses))

      // val all = x.flatten
    val all = ((x.flatten).toList ++ (stat.flatten.toList)).toArray
    // val all = ((stat.flatten).toList ++ (x.flatten.toList)).toArray

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(n <- nurses) {
        cp.add(regular((for{d <- days} yield x(n)(d)), automaton))
      }

      // special constraints:
      //   for each day there must be 
      //        at least 3 nurses with day shift
      //        at least 2 nurses with night shift
      for(d <- days) {

        // This gcc was suggested by Pierre Schaus
        // (3,2,0) means require at least 3 at day shift, at least 2 at night shift
        cp.add(gcc(Array.tabulate(num_nurses)(x(_)(d)),0 to 2,Array(3,2,0),Array(num_nurses,num_nurses,num_nurses)))

        // for the statistics
        for(s <- shifts_a) {
          cp.add(stat(d)(s) == sum(for{n <- nurses} yield(x(n)(d) === s)))
        }
      }


     } exploration {

      cp.binaryFirstFail(all)
         
      for(n <- nurses) {
        print("Nurse " + n + ": ")
        var vv = Array(0,0,0) // shift stats
        var wd = 0
        for(d <- days) {
          val v = x(n)(d).getValue()
          print(days_str(v) + " ")
          vv(v) += 1
          if (v <= night_shift) {
            wd += 1
          }
        }
        println("#workdays:" + wd + "  d:" + vv(0) + "  n:" + vv(1) + "  o:" + vv(2))
      }
      println("\nStats:")
      println("        d  n  o")
      for(d <- days) {
        print("Day " + "%2d".format(d) + ":")
        for(s <- shifts_a) {
          print(stat(d)(s) + " ")
        }
        println()
      }
      println()

      numSols += 1

      if (num_to_show > 0 && numSols >= num_to_show) {
         cp.stop()
      }
      
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
