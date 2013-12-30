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



import scala.math

/**
 *
 * A Dudeney Numbers is a positive integer that is a perfect cube such that the sum of
 * its decimal digits is equal to the cube root of the number.
 * There are only six Dudeney Numbers and those are very easy to find with CP.
 * @author Pierre Schaus pschaus@gmail.com
 */
object Dudeney {

  def main(args: Array[String]) {
    val n = 5

    val cp = new CPSolver()

    val x = (0 until n).map(v => CPVarInt(0 to 9)(cp))
    val nb = CPVarInt(1 to math.pow(10, n).toInt - 1)(cp)
    val s = CPVarInt(1 to 9 * n)(cp)

    cp.solve subjectTo {
      cp.add(nb == (s mul s mul s))
      cp.add(sum(0 until n)(i => x(i) * (math.pow(10, (n - i - 1)).toInt)) == nb)
      cp.add(sum(x) == s)
    } search {
      binaryFirstFail(x)
    } onSolution {
      println(nb.value)
    }
    println(cp.start())

  }

}