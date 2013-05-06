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
/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp.multiobjective

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.visual.VisualFrame
import oscar.visual.PlotPareto
import java.awt.Color
import oscar.util.reader.QAPReader._
import oscar.util.time

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 *
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object BiQuadraticAssignment extends App {

  val seed = 0
  val n = 10
  val N = 0 until n

  val rand = new scala.util.Random(seed)
  val dataFile = "data/mo-qap/KC10-2fl-1rl.dat"
  val solutionFile = "data/mo-qap/solutions/KC10-2fl-1rl.PO.txt"

  // Data
  // ----
  val (d, w1, w2) = read(dataFile, n)
  val xinit = rand.shuffle((0 until n).toList).toArray
  val obj1init = (for (i <- N; j <- N) yield d(xinit(i))(xinit(j)) * w1(i)(j)).sum
  val obj2init = (for (i <- N; j <- N) yield d(xinit(i))(xinit(j)) * w2(i)(j)).sum

  // Visualization
  // -------------
  val f = new VisualFrame("Knapsack MO", 1, 2)
  val paretoPlot = new PlotPareto(nbPareto = 2, objMax1 = false, objMax2 = false)
  val sols = readSolutions(solutionFile, n)
  for ((_, sol) <- sols) paretoPlot.insert(sol(0), sol(1), 1)
  f.add(paretoPlot)
  f.pack()

  // Model
  // -----
  val cp = CPSolver()
  cp.silent = true

  val x: Array[CPVarInt] = Array.fill(n)(CPVarInt(cp, N))
  val dist = Array.tabulate(n, n) { case (i, j) => d(x(i))(x(j)) }
  val obj1 = sum(N, N)((i, j) => dist(i)(j) * w1(i)(j))
  val obj2 = sum(N, N)((i, j) => dist(i)(j) * w2(i)(j))

  def heuristic(w: Array[Array[Int]]): (Int, Int) = {
    val (weight, i: Int, j: Int) =
      (for (i <- N; j <- N; if (!x(i).isBound))
        yield (w(i)(j) + w(j)(i), i, j)).max
    val (dist, vi: Int) =
      (for (vi <- x(i); vj <- x(j))
        yield (d(vi)(vj) + d(vi)(vj), vi)).min
    (i, vi)
  }

  cp.addDecisionVariables(x)

  cp.paretoMinimize(obj1, obj2) subjectTo {
    cp.add(allDifferent(x), Strong)
  }

  cp.exploration {
    while (!allBounds(x)) {
      val (i, v) = heuristic(if (rand.nextBoolean) w1 else w2)
      cp.branch(cp.post(x(i) == v))(cp.post(x(i) != v))
    }
    paretoPlot.insert(obj1.value, obj2.value)
  }

  println("search...")
  val t = time {
    cp.run()
  }

  println("time " + t)
  println("size " + cp.nonDominatedSolutions.size)
}
