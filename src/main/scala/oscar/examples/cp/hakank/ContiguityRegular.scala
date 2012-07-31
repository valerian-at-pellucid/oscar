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
import oscar.cp.constraints._
import collection.mutable._
import scala.collection.JavaConversions._

/**

  Global constraint Contiguity using regular in Oscar.

  Global constraint contiguity using Transition
  
  This version use the built-in TransitionConstraint.
  
  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 0 or 1.
  In addition, all variables assigned to value 1 appear contiguously.
  
  Example:
  (<0, 1, 1, 0>)
  
  The global_contiguity constraint holds since the sequence 0 1 1 0 contains
  no more than one group of contiguous 1.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object ContiguityRegular {


  def MyContiguity(cp: CPSolver, x: Array[CPVarInt]) {

    // all states are accepting states
    val accepting_states: Set[java.lang.Integer] = Set(0,1,2)

    // The regular expression 0*1*0*

    // Note: Oscar want the transitions in this order
    // {state, next state, emitting letter}
    val transition_tuples = Array(Array(0, 0, 0),
                                  Array(0, 1, 1),
                                  Array(1, 2, 0),
                                  Array(1, 1, 1),
                                  Array(2, 2, 0))

    val initial_state = 0
    val num_states = 3
    val num_letters = 2

    val automaton = new Automaton(num_states, num_letters, initial_state, accepting_states)

    transition_tuples.foreach(a => automaton.addTransition(a(0), a(1), a(2)))

    cp.add(regular(x, automaton))

  }

  def main(args: Array[String]) {

    val cp = CPSolver()

    var n = 7
    if (args.length > 0) {
      n = args(0).toInt
    }

    println("n:" + n)

    //
    // variables
    //
    val reg_input = Array.fill(n)(CPVarInt(cp, 0 to 1))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      MyContiguity(cp, reg_input)

    } exploration {
       
      cp.binary(reg_input)

      println(reg_input.mkString(""))

      numSols += 1
       
    }

    println("\nIt was " + numSols + " solutions.")

    cp.printStats()

  }

}
