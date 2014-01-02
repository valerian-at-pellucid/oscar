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
package oscar.cp.constraints.implementations

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._
import scala.Array.canBuildFrom

/**
 * Sequence
 * @author Pierre Schaus pschaus@gmail.com
 */
class SequenceDecomposition(val xinit: Array[CPVarInt], val values: Set[Int], val l: Int, min: Int, max: Int) extends Constraint(xinit(0).s, "Sequence") {

  if (values.size <= 0) throw new IllegalArgumentException("Sequence: values.size <= 0")
  if (l > xinit.size) throw new IllegalArgumentException("Sequence: l > xinit.size")
  if (l == xinit.size) println("Sequence: warning: you should use a gcc instead of a sequence here")
  if (l < 0) throw new IllegalArgumentException("Sequence: l < 0")
  if (min > max) throw new IllegalArgumentException("Sequence: min > max")
  if (min < 0) throw new IllegalArgumentException("Sequence: min < 0")
  if (max >  l) throw new IllegalArgumentException("Sequence: max > l")
  
  override def setup(strenght: CPPropagStrength): CPOutcome = {
    val x = xinit.map(_.isIn(values))

    // cumulatedCounters[i] = x[0]+x[1]+...+x[i]
    val cumulatedCounters: Array[CPVarInt] = Array.fill(x.size)(null)
    cumulatedCounters(0) = x(0)
    for (i <- 1 until x.size) {
      cumulatedCounters(i) = x(i) + cumulatedCounters(i - 1)
    }

    // partial sums Pij = x[i]+...+x[j]
    val P: Array[Array[CPVarInt]] = Array.fill(x.size, x.size)(null)
    for (i <- 0 until x.size) {
      P(i)(i) = x(i)
      for (j <- i + 1 until (x.size.min(i + l))) {
        if (i > 0) {
          P(i)(j) = cumulatedCounters(j) - cumulatedCounters(i - 1)
        } else {
          P(i)(j) = cumulatedCounters(j)
        }
      }
    }

    for (i <- 0 until x.size) {
      for (j <- i + 1 until (x.size.min(i + l))) {
        for (m <- i until j) {
          if (s.post(new BinarySum(P(i)(m), P(m + 1)(j), P(i)(j))) == CPOutcome.Failure) {
            return CPOutcome.Failure;
          }
        }
      }
      if (i <= x.size - l) {
        if (s.post(P(i)(i + l - 1) >= min) == CPOutcome.Failure) {
          return CPOutcome.Failure;
        }
        if (s.post(P(i)(i + l - 1) <= max) == CPOutcome.Failure) {
          return CPOutcome.Failure;
        }
      }
    }
    return CPOutcome.Success
  }

}


