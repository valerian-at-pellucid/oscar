/*******************************************************************************
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
 ******************************************************************************/
package oscar.cp.constraints

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._
import oscar.cp.modeling._
import oscar.algo.reversible.ReversibleSparseSet

/**
 * Ensures that succ represents a valid circuit. <br>
 * succ(i) represents the city visited after city i. Each city is visited once and
 * there is only one tour.<br>
 * Available propagation strengths are Weak, Medium and Strong.
 * Weak = elements + circuit + alldiff (AC)
 * Medium = Weak + minAssignment
 * Strong = Medium + Held&Karp Lower-Bounds
 * @param succ
 * @see CPPropagStrength
 * @author Pierre Schaus pschaus@gmail.com
 */
class MinCircuit(val succ: Array[CPIntVar], val distMatrix: Array[Array[Int]], obj: CPIntVar, addPredModel: Boolean = true) extends Constraint(obj.store, "MinCircuit") {

  
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    val n = succ.size
    val distMatrixSucc = Array.tabulate(n,n)((i,j) => distMatrix(i)(j))
    
    
    
    val pred = Array.fill(n)(CPIntVar(0 until n)(s))

    if (s.post(new Circuit(succ,false), Strong) == Failure) return Failure

    if (s.post(new Sum((0 until n).map(i => distMatrixSucc(i)(succ(i))), obj)) == Failure) return Failure

    if (l == CPPropagStrength.Medium || l == CPPropagStrength.Strong) {
      if (s.post(minAssignment(succ, distMatrixSucc, obj)) == Failure) {
        println("failure min assignment")
        return Failure
      }
    }

    if (l == CPPropagStrength.Strong) {
      if (s.post(new AsymetricHeldKarp(succ, distMatrixSucc, obj)) == Failure) {
        return Failure
      }
    }

    if (addPredModel) {
      val distMatrixPred = Array.tabulate(n, n)((i, j) => distMatrixSucc(j)(i))
      val pred = Array.fill(n)(CPIntVar(0 until n)(s))
      if (s.post(new Inverse(pred, succ), l) == Failure) return Failure
      if (s.post(new MinCircuit(pred, distMatrixPred, obj, false),l) == Failure) return Failure
    }

    return CPOutcome.Success
  }

}


