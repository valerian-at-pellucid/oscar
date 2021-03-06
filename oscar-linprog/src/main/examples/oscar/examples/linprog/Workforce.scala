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
package oscar.examples.linprog


import oscar.linprog.modeling._
import oscar.algebra._

/**
 * Assign workers to shifts while satisfying requirements for that day.
 * Each worker may or may not be available on a particular day.
 * The objective is to minimize the total pay costs
 * (Example Inspired from Gurobi Website.)
 * @author Pierre Schaus pschaus@gmail.com
 */

object Workforce extends MIPModel(LPSolverLib.glpk) with App {

  val Shifts = 0 to 13
  val Workers = 0 to 6
  val shiftRequirements = Array(3, 2, 4, 1, 5, 2, 4, 2, 2, 3, 4, 5, 3, 5)
  val pay = Array(10, 12, 10, 8, 8, 9, 11) // salary of each persons when they are scheduled
  val maxNbShift = 7 // maximum number of shift a worker can be assigned to in the schedule

  val availability =
    Array(Array(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1),
      Array(1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0),
      Array(0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1),
      Array(0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1),
      Array(1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
      Array(1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1),
      Array(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

  val assigned = Array.tabulate(Workers.size, Shifts.size)((_, _) => MIPFloatVar(0 to 1))

  minimize(sum(Workers, Shifts)((w, s) => assigned(w)(s) * pay(w)))
  for (s <- 0 until Shifts.size) {
    add(sum(Workers)(w => assigned(w)(s) * availability(w)(s)) == shiftRequirements(s))
  }
  for (w <- Workers) {
    add(sum(Shifts)(s => assigned(w)(s)) <= maxNbShift)
  }
  start()
  
  println("objective: " + objectiveValue.get)
  for (s <- 0 until Shifts.size) {
    println("Day " + s + " workers: " + Workers.filter(assigned(_)(s).value.get == 1))
  }
  release()

}
