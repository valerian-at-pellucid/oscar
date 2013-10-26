package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.SweepMaxCumulative

object AlternativeTask extends App {

  val durationsData = Array(2, 2, 3, 3, 3, 4, 4, 5, 6, 7)
  val demandsData1  = Array(1, 2, 1, 3, 2, 1, 1, 2, 3, 2)
  val demandsData2  = Array(2, 3, 2, 1, 2, 3, 2, 3, 1, 3)
  val horizon = durationsData.sum
  val capaMax = 3

  val nTasks = durationsData.size
  val Tasks = 0 until nTasks

  val cp = CPSolver()
  cp.silent = true
  val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
  val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, Set(demandsData1(t), demandsData2(t))))
  val resources = Array.fill(nTasks)(CPVarInt(cp, 1 to 2))
  
  val makespan = maximum(ends)

  cp.minimize(makespan) subjectTo {
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Alternative demands
    for (t <- Tasks) {
      cp.add((resources(t) === 1) ==> (demands(t) === demandsData1(t)))
      cp.add((resources(t) === 2) ==> (demands(t) === demandsData2(t)))
    }
    // Cumulative
    cp.add(SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capaMax), 1))
    cp.add(SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capaMax), 2))

  } exploration {
    cp.binaryFirstFail(resources)
    cp.binaryFirstFail(starts)
    
    println("Makespan of " + makespan.value)
    for (t <- Tasks) {
      print("Task " + t)
      print(" of duration " + durations(t).value)
      print(" and demand " + demands(t).value)
      print(" is executed on resource " + resources(t).value)
      println(" at time " + starts(t).value)
    }
    println()
  }

  cp.run()
  cp.printStats
}