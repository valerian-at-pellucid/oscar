package oscar.cbls.jobshop.rich

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

import oscar.cbls.jobshop.algo.QuickXplain
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.{Solution, Model}

class JobShopSolver(p: Planning, Verbose: Boolean = true) extends SearchEngine {
  val model: Model = p.model

  class FlatteningHeuristics()
  case class EarliestFirst() extends FlatteningHeuristics
  case class WorseFirst() extends FlatteningHeuristics
  case class Random() extends FlatteningHeuristics

  /**This solves the jobshop by iterative relaxation and flattening
   * @param MaxIt the max number of iterations of the search
   * @param Stable the number of no successice noimprove that will cause the search to stop
   */
  def Solve(MaxIt: Int, Stable: Int, flatteningheursitics: FlatteningHeuristics = WorseFirst(), NbRelax: Int = 4, PkillPerRelax: Int = 50) {

    var it: Int = 0

    flatteningheursitics match {
      case EarliestFirst() => FlattenEarliestFirst();
      case WorseFirst() => FlattenWorseFirst();
      case Random() => RandomFlatten();
    }

    var BestSolution: Solution = model.getSolution(true)
    if (Verbose) {
      println(p.MakeSpan)
      println("----------------")
    }

    var plateaulength = 0
    var BestMakeSpan: Int = p.MakeSpan

    while (it < MaxIt && plateaulength < Stable) {
      //iterative weakening and flattening
      it += 1

      // if(plateaulength == 20){
      // for (i <- 0 until NbRelax*2){Relax(75);}
      // println("jumping****************")
      //}else{
      for (i <- 0 until NbRelax) {
        Relax(PkillPerRelax);
      }
      //}

      flatteningheursitics match {
        case EarliestFirst() => FlattenEarliestFirst();
        case WorseFirst() => FlattenWorseFirst();
        case Random() => RandomFlatten();
      }

      println(p.MakeSpan)
      println("iteration: " + it)

      if (p.MakeSpan < BestMakeSpan) {
        BestSolution = model.getSolution(true)
        BestMakeSpan = p.MakeSpan
        plateaulength = 0
        println("Better MakeSpan found")
      } else {
        plateaulength += 1
      }
      println("----------------")
    }
    model.restoreSolution(BestSolution)
  }

  /**
   * performs the relaxation of the critical path
   * This N is our contribution; not found in iFlatiRelax papers.
   * relaxes N additional dependencies on a critical path (if n are found)
   */
  def Relax(PKill: Int) {
    //takes one node from the determining predecessors.
    def PrecedingNode(j: Task): Task = {
      if (j.DefiningPredecessors.getValue() isEmpty) null
      else p.TaskArray(selectFrom(j.DefiningPredecessors.getValue()))
      //random tie break, as it is likely that there will be few forks.
    }

    var CurrentTask: Task = PrecedingNode(p.SentinelTask)
    var PotentiallykilledNodes: List[(Task, Task)] = List.empty
    while (CurrentTask != null) {
      val Predecessor = PrecedingNode(CurrentTask)
      if (Predecessor != null && CurrentTask.AdditionalPredecessors.getValue().contains(Predecessor.TaskID)) {
        PotentiallykilledNodes = (Predecessor, CurrentTask) :: PotentiallykilledNodes
      }
      CurrentTask = Predecessor
    }
    if (PotentiallykilledNodes.isEmpty) return

    val (from, to) = selectFrom(PotentiallykilledNodes)
    if (Verbose) println("killed " + from + "->" + to)
    to.addDynamicPredecessor(from)
  }

  def RandomFlatten() {
    while (!p.EarliestOvershotResources.getValue().isEmpty) {
      val r: Resource = p.ResourceArray(selectFrom(p.EarliestOvershotResources))
      val t: Int = r.FirstOvershoot

      val TasksAndUse = r.TasksAndUse.filter((taksAndamount: (Task, Int)) => r.Use(t).contains(taksAndamount._1.TaskID))
      val Tasks: List[Task] = TasksAndUse.map((taskAndamount: (Task, Int)) => taskAndamount._1)

      val a = selectFrom(Tasks)
      val b = selectFrom(Tasks, (j: Task) => j != a)

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }

  /**implements the standard flatten procedure*/
  def FlattenWorseFirst() {
    while (!p.WorseOvershotResource.getValue().isEmpty) {
      val r: Resource = p.ResourceArray(selectFrom(p.WorseOvershotResource))
      val t: Int = selectFirst(r.HighestUsePositions)

      val TaskssAndUse = r.TasksAndUse.filter((taskAndamount: (Task, Int)) => r.Use(t).contains(taskAndamount._1.TaskID))

      val conflictSet: List[(Task, Int)] = QuickXplain(
        0,
        TaskssAndUse,
        (use: Int, taskAndamount: (Task, Int)) => use + taskAndamount._2,
        (use: Int, taskAndamount: (Task, Int)) => use - taskAndamount._2,
        (use: Int) => use > r.MaxAmount
      )

      val conflictTasks: List[Task] = conflictSet.map((takAndamount: (Task, Int)) => takAndamount._1)

      //println("flatten length: " + TaskssAndUse.length)

      val (a, b) = selectMax2(conflictTasks, conflictTasks,
        (a: Task, b: Task) => (b.LatestEndDate - a.EarliestStartDate),
        (a: Task, b: Task) => a != b)

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }

  def FlattenEarliestFirst() {
    while (!p.EarliestOvershotResources.getValue().isEmpty) {
      val r: Resource = p.ResourceArray(selectFrom(p.EarliestOvershotResources))
      val t: Int = r.FirstOvershoot

      //the two selected tasks a,b must belong to a minimal conflict set
      //and they must maximize lsd(b)-esd(a)  //pq pas led(b) - esd(a)??
      //then insert a->b

      //modification par rapport a l'heuristique:
      //uniquement b doit appartenir au conflict set.
      //et on maximise led(b) - esd(a)

      val TaskssAndUse = r.TasksAndUse.filter((taskAndamount: (Task, Int)) => r.Use(t).contains(taskAndamount._1.TaskID))
      val Tasks: List[Task] = TaskssAndUse.map((taskAndamount: (Task, Int)) => taskAndamount._1)

      val conflictSet: List[(Task, Int)] = QuickXplain(
        0,
        TaskssAndUse,
        (use: Int, taskAndamount: (Task, Int)) => use + taskAndamount._2,
        (use: Int, taskAndamount: (Task, Int)) => use - taskAndamount._2,
        (use: Int) => use > r.MaxAmount
      )

      val conflictTasks: List[Task] = conflictSet.map((taskAndamount: (Task, Int)) => taskAndamount._1)

      val (a, b) = selectMax2(Tasks, conflictTasks,
        (a: Task, b: Task) => (b.LatestStartDate - a.EarliestEndDate), //c'est pas l'inverse?
        (a: Task, b: Task) => a != b)

      if (Verbose) println("added " + a + "->" + b)
      b.addDynamicPredecessor(a)
    }
  }
}


