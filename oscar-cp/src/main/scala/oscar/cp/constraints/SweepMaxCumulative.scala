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
 * *****************************************************************************/
package oscar.cp.constraints

import scala.math.max
import scala.math.min
import oscar.cp.core.CPStore
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPSolver
import oscar.algo.SortUtils.stableSort

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class SweepMaxCumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) extends Constraint(starts.head.store, "MaxSweepCumulative") {

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  // Contains all the events representing the tasks (needs to be initialized)
  private val eventPointSeries = new Array[Event](nTasks * 3)

  // Current size of eventPointSeries
  private var nEvents = 0

  // Current position of the sweep line
  private var delta = 0
  // Sum of the height of the tasks that overlap the sweep line
  private var consSumHeight = 0
  // Sum of the height of the tasks that overlap the sweep line
  private var capaSumHeight = 0
  // Number of tasks that overlap the sweep line
  private var nCurrentTasks = 0
  // Number of tasks in stackPrune
  private var nTasksToPrune = 0

  // Tasks that could intersect the sweep line
  private val stackPrune = new Array[Int](nTasks)
  // Contribution of all the tasks that are added to consSumHeight
  private val consContrib = new Array[Int](nTasks)
  // Contribution of all the tasks that are added to capaSumHeight
  private val capaContrib = new Array[Int](nTasks)

  // Contains all the possible events of each task (used for speed-up)
  private val eventList = Array.tabulate(nTasks) { e => new EventList(e) }

  private def generateCheck(i: Int): Unit = {}

  private def generateProfileBad(i: Int): Boolean = {

    if (demands(i).min > 0) {

      eventPointSeries(nEvents) = eventList(i).sBadProfile(0, demands(i).min)
      nEvents += 1
      eventPointSeries(nEvents) = eventList(i).eBadProfile(0, demands(i).min)
      nEvents += 1

      return true
    }

    return false
  }

  private def generateProfileGood(i: Int): Boolean = {

    if (demands(i).min < 0) {

      eventPointSeries(nEvents) = eventList(i).sGoodProfile(0, demands(i).min)
      nEvents += 1
      eventPointSeries(nEvents) = eventList(i).eGoodProfile(0, demands(i).min)
      nEvents += 1

      return true
    }

    return false
  }

  private def consistencyCheck: Boolean = (capaSumHeight > capacity.max)

  private def mandatoryCheck(t: Int): Boolean = capaSumHeight - capaContrib(t) > capacity.max

  private def forbidenCheck(t: Int): Boolean = capaSumHeight - capaContrib(t) + demands(t).min > capacity.max

  override def setup(l: CPPropagStrength): CPOutcome = {

    priorityL2 = 0

    val oc = propagate()

    if (oc == CPOutcome.Suspend) {
      capacity.callPropagateWhenBoundsChange(this)
      for (i <- Tasks) {
        if (!starts(i).isBound) starts(i).callPropagateWhenBoundsChange(this)
        if (!durations(i).isBound) durations(i).callPropagateWhenBoundsChange(this)
        if (!ends(i).isBound) ends(i).callPropagateWhenBoundsChange(this)
        if (!demands(i).isBound) demands(i).callPropagateWhenBoundsChange(this)
        if (!resources(i).isBound) resources(i).callPropagateWhenDomainChanges(this)
      }
    }

    return oc
  }

  override def propagate(): CPOutcome = {

    // Generates events
    if (!generateEventPointSeries())
      return CPOutcome.Suspend

    // Performs a sweep on the events
    if (sweepAlgorithm() == CPOutcome.Failure)
      return CPOutcome.Failure

    return CPOutcome.Suspend
  }

  private def generateEventPointSeries(): Boolean = {

    // True if a profile event has been generated
    var profileEvent = false

    // Reset eventPointSeries
    nEvents = 0

    var i = 0
    while (i < nTasks) {

      if (starts(i).max < ends(i).min && resources(i).isBoundTo(id)) {

        // Check
        generateCheck(i)

        // Profile (Bad : on compulsory part)
        profileEvent |= generateProfileBad(i)
      }

      if (resources(i).hasValue(id)) {

        // Profile (Good : on entire domain)
        profileEvent |= generateProfileGood(i)

        // Pruning (if something is not fixed)
        if (!starts(i).isBound || !ends(i).isBound || !resources(i).isBoundTo(id) || !demands(i).isBound) {

          eventPointSeries(nEvents) = eventList(i).sPruning
          nEvents += 1
        }
      }

      i += 1
    }

    profileEvent
  }

  private def resetSweepLine = {

    delta = 0
    consSumHeight = 0
    capaSumHeight = 0
    nCurrentTasks = 0
    nTasksToPrune = 0

    for (i <- Tasks) {
      consContrib(i) = 0
      capaContrib(i) = 0
    }
  }

  private def sweepAlgorithm(): CPOutcome = {

    resetSweepLine

    // Sort events by increasing date
    stableSort(eventPointSeries, 0, nEvents, (a: Event, b: Event) => a.date < b.date)

    // First position of the sweep line
    var delta = eventPointSeries(0).date

    var i = 0
    while (i < nEvents) {

      val event = eventPointSeries(i)

      if (event.eType != EventType.pruning) {

        // If we have considered all the events at the previous position
        // of the sweep line
        if (delta != event.date) {

          // Consistency check
          if (consistencyCheck)
            return CPOutcome.Failure

          // Pruning (this could reduce the size of stackPrune)
          if (prune(delta, event.date - 1) == CPOutcome.Failure)
            return CPOutcome.Failure

          // Moves the sweep line
          delta = event.date
        }

        if (event.eType == EventType.profile) {

          // Adjusts height consumption
          consSumHeight += event.cons
          consContrib(event.task) += event.cons

          // Adjusts height capacity
          capaSumHeight += event.capa
          capaContrib(event.task) += event.capa

        } else if (event.eType == EventType.check) {

          // Number of overlapping tasks
          nCurrentTasks += event.cons
        }
      } else {
        stackPrune(nTasksToPrune) = event.task
        nTasksToPrune += 1
      }

      i += 1
    }

    // Checks consistency
    if (consistencyCheck)
      return CPOutcome.Failure

    // Final pruning
    if (prune(delta, delta) == CPOutcome.Failure)
      return CPOutcome.Failure

    return CPOutcome.Suspend
  }

  private def prune(low: Int, up: Int): CPOutcome = {

    // Used to adjust stackPrune
    var nRemainingTasksToPrune = 0

    var i = 0
    while (i < nTasksToPrune) {

      val t = stackPrune(i)

      // Pruning on tasks that must be discarded to respect consistency
      if (pruneForbiden(t, id, low, up) == CPOutcome.Failure)
        return CPOutcome.Failure

      // Pruning on tasks that are mandatory to respect consistency
      if (pruneMandatory(t, id, low, up) == CPOutcome.Failure)
        return CPOutcome.Failure

      // Adjusts the height's consumption of the tasks
      if (pruneConsumption(t, id, low, up) == CPOutcome.Failure)
        return CPOutcome.Failure

      // If the task is still in conflict, we keep it
      if (!(ends(t).max <= up + 1)) {
        stackPrune(nRemainingTasksToPrune) = t
        nRemainingTasksToPrune += 1
      }

      i += 1
    }

    // Adjusting stackPrune
    nTasksToPrune = nRemainingTasksToPrune

    return CPOutcome.Suspend
  }

  private def pruneMandatory(t: Int, r: Int, low: Int, up: Int): CPOutcome = {

    // Checks if the task is mandatory to respect consistency
    if (!mandatoryCheck(t))
      return CPOutcome.Suspend

    // Fix the activity to the resource r
    if (resources(t).assign(r) == CPOutcome.Failure)
      return CPOutcome.Failure

    // Adjust the EST of the activity
    if (starts(t).updateMin(up - durations(t).max + 1) == CPOutcome.Failure)
      return CPOutcome.Failure

    // Adjust the LST of the activity
    if (starts(t).updateMax(low) == CPOutcome.Failure)
      return CPOutcome.Failure

    // Adjust the LCT of the activity
    if (ends(t).updateMax(low + durations(t).max) == CPOutcome.Failure)
      return CPOutcome.Failure

    // Adjust the ECT of the activity
    if (ends(t).updateMin(up + 1) == CPOutcome.Failure)
      return CPOutcome.Failure

    // Adjust the minimal duration of the activity
    if (durations(t).updateMin(min(up - starts(t).max + 1, ends(t).min - low)) == CPOutcome.Failure)
      return CPOutcome.Failure

    return CPOutcome.Suspend
  }

  private def pruneForbiden(t: Int, r: Int, low: Int, up: Int): CPOutcome = {

    // Checks if the task must be discarded to respect consistency
    if (forbidenCheck(t)) {

      if (ends(t).min > low && starts(t).max <= up && durations(t).min > 0) {

        if (resources(t).removeValue(r) == CPOutcome.Failure)
          return CPOutcome.Failure

      } else if (resources(t).isBoundTo(r)) {

        if (durations(t).min > 0) {

          if (pruneInterval(low - durations(t).min + 1, up, starts(t)) == CPOutcome.Failure)
            return CPOutcome.Failure
        }

        if (!durations(t).isBound) {

          if (durations(t).min > 0) {
            if (pruneInterval(low + 1, up + durations(t).min, ends(t)) == CPOutcome.Failure)
              return CPOutcome.Failure
          }

          val maxD = max(max(low - starts(t).min, ends(t).max - up - 1), 0)

          if (durations(t).updateMax(maxD) == CPOutcome.Failure)
            return CPOutcome.Failure
        }
      }
    }

    return CPOutcome.Suspend
  }

  private def pruneConsumption(t: Int, r: Int, low: Int, up: Int): CPOutcome = {

    if (resources(t).isBoundTo(r) && ends(t).min > low && starts(t).max <= up && durations(t).min > 0) {

      if (demands(t).updateMax(capacity.max - (capaSumHeight - capaContrib(t))) == CPOutcome.Failure)
        return CPOutcome.Failure
    }

    return CPOutcome.Suspend
  }

  private def pruneInterval(low: Int, up: Int, v: CPIntVar): CPOutcome = {

    assert(low <= up)
    if (low <= v.min && up <= v.max) {
      v.updateMin(up + 1)
    } else if (up >= v.max && low >= v.min) {
      v.updateMax(low - 1)
    } else CPOutcome.Suspend

    return CPOutcome.Suspend
  }

  private object EventType {
    val check = 0
    val profile = 1
    val pruning = 2

    def eventToString(i: Int) = {
      i match {
        case 0 => "check"
        case 1 => "profile"
        case 2 => "pruning"
        case _ => "unknown event"
      }
    }
  }

  private class Event(e: Int, t: Int, private var d: Int, private var consomation: Int, private var capacity: Int) {

    def date = d
    def eType = e
    def cons = consomation
    def capa = capacity
    def task = t

    def date_=(x: Int) { d = x }
    def cons_=(x: Int) { consomation = x }
    def capa_=(x: Int) { capacity = x }

    override def toString = { "<" + EventType.eventToString(e) + ", " + t + ", " + d + ", " + capa + ", " + cons + ">" }
  }

  private class EventList(t: Int) {

    val sCheckEv: Event = new Event(EventType.check, t, 0, 1, 1)
    val eCheckEv: Event = new Event(EventType.check, t, 0, -1, -1)
    val sBadProfileEv: Event = new Event(EventType.profile, t, 0, 0, 0)
    val eBadProfileEv: Event = new Event(EventType.profile, t, 0, 0, 0)
    val sGoodProfileEv: Event = new Event(EventType.profile, t, 0, 0, 0)
    val eGoodProfileEv: Event = new Event(EventType.profile, t, 0, 0, 0)
    val PruningEv: Event = new Event(EventType.pruning, t, 0, 0, 0)

    def sCheck: Event = {

      sCheckEv.date = starts(sCheckEv.task).max
      return sCheckEv
    }

    def eCheck: Event = {

      eCheckEv.date = ends(eCheckEv.task).min
      return eCheckEv
    }

    def sBadProfile(consInc: Int, capaInc: Int): Event = {

      sBadProfileEv.date = starts(sBadProfileEv.task).max
      sBadProfileEv.capa = capaInc
      sBadProfileEv.cons = consInc
      return sBadProfileEv
    }

    def eBadProfile(consInc: Int, capaInc: Int): Event = {

      eBadProfileEv.date = ends(eBadProfileEv.task).min
      eBadProfileEv.capa = -capaInc
      eBadProfileEv.cons = -consInc
      return eBadProfileEv
    }

    def sGoodProfile(consInc: Int, capaInc: Int): Event = {

      sGoodProfileEv.date = starts(sGoodProfileEv.task).min
      sGoodProfileEv.capa = capaInc
      sGoodProfileEv.cons = consInc
      return sGoodProfileEv
    }

    def eGoodProfile(consInc: Int, capaInc: Int): Event = {

      eGoodProfileEv.date = ends(eGoodProfileEv.task).max
      eGoodProfileEv.capa = -capaInc
      eGoodProfileEv.cons = -consInc
      return eGoodProfileEv
    }

    def sPruning: Event = {

      PruningEv.date = starts(PruningEv.task).min
      return PruningEv
    }
  }
}

object SweepMaxCumulative {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): SweepMaxCumulative = {
    val nTasks = starts.size
    if (nTasks == 0) throw new Exception("no tasks")
    else if (ends.size != nTasks) throw new Exception("the number of end variables should be " + nTasks)
    else if (durations.size != nTasks) throw new Exception("the number of duration variables should be " + nTasks)
    else if (demands.size != nTasks) throw new Exception("the number of demand variables should be " + nTasks)
    else if (resources.size != nTasks) throw new Exception("the number of resource variables should be " + nTasks)
    else if (durations.exists(_.min < 0)) throw new Exception("durations have to be superior or equal to 0")
    else new SweepMaxCumulative(starts, durations,ends, demands, resources, capacity, id)
  }
} 
