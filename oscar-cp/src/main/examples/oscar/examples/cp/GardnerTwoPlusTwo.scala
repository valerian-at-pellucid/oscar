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
package oscar.examples.cp

import oscar.cp.modeling._

import oscar.cp.core._

/**
 *
 * Martin Gardner Problem:
 *
 * In this addition problem, each letter stands for a different digit.
 *
 *   T W O
 * + T W O
 * --------
 * F O U R
 *
 * If T = 7 and the letter O represents an even number, what is the only possible value for W
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object GardnerTwoPlusTwo extends App {

  val cp = CPSolver()
  val T = CPVarInt(0 to 9)(cp)
  val W = CPVarInt(0 to 9)(cp)
  val O = CPVarInt(Set(0, 2, 4, 6, 8))(cp) // even number
  val F = CPVarInt(0 to 9)(cp)
  val U = CPVarInt(0 to 9)(cp)
  val R = CPVarInt(0 to 9)(cp)

  cp.solve subjectTo {
    cp.add((T * 100 + W * 10 + O) * 2 == F * 1000 + O * 100 + U + 10 + R)
    cp.add(allDifferent(Array(T, W, O, F, U, R)), Strong)
  } search {
    binaryFirstFail(Seq(T, W, O, F, U, R))
  } onSolution {
    println("T:" + T + " W:" + W + " O:" + O + " F:" + F + " U:" + U + " R:" + R)
  }
  println(cp.start())
}