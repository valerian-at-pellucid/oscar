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

  Decomposition of circuit constraint in Oscar.

  Here we implement a decomposition of circuit (circuit_me)
  and also a path extraction for the circuit (circuit_path).

  Please note that Oscar has a built-in circuit/1. This
  is just an etude...

  Comparison (where the results redirects to a file)

  Using my decomposition (circuit_me) took 47.103s
  with these statistics:

    It was 362880 solutions.
    (time(ms),46091)
    (#bkts,362879)
    (time in fix point(ms),15771)
    (time in trail restore(ms),6487)
    (max trail size,2076)


  Using the built-in circuit (circuit) took 44.652
  with these statistics (slightly better):

     It was 362880 solutions.
     (time(ms),43639)
     (#bkts,362879)
     (time in fix point(ms),13336)
     (time in trail restore(ms),5682)
     (max trail size,1181)

  Without any output of the solutions it's much faster (~ factor 2).

  My decomposition (circuit_me) took 25.178s
     It was 362880 solutions.
     (time(ms),24232)
     (#bkts,362879)
     (time in fix point(ms),14445)
     (time in trail restore(ms),6485)
     (max trail size,2076)

  Built-in (circuit) took 22.406s
     It was 362880 solutions.
     (time(ms),21439)
     (#bkts,362879)
     (time in fix point(ms),12849)
     (time in trail restore(ms),5674)
     (max trail size,1181)


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object CircuitTest extends CPModel {


  /*
   * circuit_me(x) 
   *
   * Ensures that x is an circuit.
   *
   * Note: the domain of x must be 1..n, where n is the
   * length of x
   * 
   */
  def circuit_me(cp: CPSolver, x: Array[CPVarInt]) = {
    
    val len = x.length
    val z = Array.tabulate(len)(i=> CPVarInt(cp, 0 to len-1))

    cp.add(alldifferent(x), Strong)
    cp.add(alldifferent(z), Strong)

    cp.add(z(0) == x(0))

    // then put the orbit of x(0) in z(1..n-1)
    for(i <- 1 until len) {
      cp.add(element(x, z(i-1), z(i)), Strong)
    }

    // for(i <- lb until len-1) {
    //    cp.add(z(i) != 0)
    // }
    
    cp.add(z(len-1) == 0)

  } // end circuit_me


  /*
   * circuit_path(x, p)
   * 
   * Extract the path p from a circuit x.
   * We assume that the path starts from 1
   * 
   */
  def circuit_path(cp: CPSolver, x: Array[CPVarInt], p:Array[CPVarInt]) = {

    val len = x.length

    cp.add(alldifferent(p), Strong)
    cp.add(p(0) == 0) // path always starts with 1
    for(i <- 1 until len) {
      cp.add(element(x, p(i-1), p(i)), Strong) 
    }

  } // end circuit_path




  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6

    //
    // variables
    //

    // Note: Here we use domain of 0..n-1
    val x = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val p = Array.fill(n)(CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {

      // constraints

      // cp.add(circuit(x), Strong) // use the built-in
      circuit_me(cp, x)
      circuit_path(cp, x, p)
 

    } exploration {
       
      cp.binaryFirstFail(x)

      println("\nCircuit: "+ x.mkString(""))
      println("Path   : " + p.mkString(""))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
