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
package oscar.examples.cbls

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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.Model
import io.Source
import oscar.cbls.search.StopWatch
import oscar.visual.{VisualUtil, VisualFrame}
import oscar.visual.scheduling.VisualProfile
import oscar.visual.scheduling.VisualGanttChart
import oscar.cbls.scheduling._

/**this class loads JobShop problems as defined in
 * http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/jobshop1.txt
 * one problem per file
 */
object GenericJobShopTest extends StopWatch with App {

  if (args.size == 0) {
    println("usage: GenericJobShopTest fileName maxit stable")
    sys.exit()
  }

  val file = Source.fromFile(args(0))

  val MaxIt = args(1).toInt
  val Stable = args(2).toInt
  val maxDuration = args(3).toInt

  val src: Array[String] = file.mkString.split("\\s+")
  file.close()

  println("JobShop(" + args(0) + ")")
  val WordReader = src.iterator

  val JobCount = WordReader.next().toInt
  val MachineCount = WordReader.next().toInt

  println("MaxIt: " + MaxIt)
  println("Stable: " + Stable)
  println("Jobs: " + JobCount)
  println("Machines: " + MachineCount)
  println("Tasks: " + JobCount * MachineCount + "\n")

  val model = new Model(false, None, false)
  val planning = new Planning(model, maxDuration)

  val MachineArray: Array[CumulativeResource] = Array.tabulate(MachineCount)(MachineID
    => CumulativeResource(planning, 1, "Machine" + MachineID))

  for (JobID <- 0 until JobCount) {
    var PreviousTask: Activity = null
    for (TaskID <- MachineArray.indices) {
      val MachineID = WordReader.next().toInt
      val Duration = WordReader.next().toInt

      val NewTask = Activity(Duration, planning, "Task_" + TaskID + "_of_Job_" + JobID)
      NewTask uses 1 ofResource MachineArray(MachineID)

      if (PreviousTask != null)
        PreviousTask precedes NewTask

      PreviousTask = NewTask
    }
  }
  startWatch()
  planning.close()

  val solver = new IFlatIRelax(planning, false)

  model.close()
  planning.getVisual()

  println("start search")
  //println(model.dumpToDot(true,true))
  solver.Solve(MaxIt, Stable, solver.WorseFirst(), 3, 20)

  println("run time: " + getWatch)
  println(planning.toAsciiArt)
}
