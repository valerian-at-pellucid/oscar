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
package oscar.cbls.scheduling

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

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar}
import oscar.cbls.invariants.lib.set.{Inter, Union}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.lib.minmax.{MinArray, ArgMaxArray}

class SuperTask(start: Activity, end: Activity, override val name: String = "")
  extends Activity(new IntVar(start.planning.model, 0, start.planning.maxduration, start.duration.value, "duration of " + name),
    start.planning, name) {

  start precedes end

  override def post() {

    start.post()
    end.post()

    AdditionalPredecessors = start.AdditionalPredecessors

    AllPrecedingActivities = start.AllPrecedingActivities

    EarliestStartDate <== start.EarliestStartDate

    DefiningPredecessors = start.DefiningPredecessors

    PotentiallyKilledPredecessors = start.PotentiallyKilledPredecessors

    AllSucceedingActivities = new IntSetVar(planning.model, 0, planning.activityCount - 1, "succeeding_jobs")

    LatestEndDate <== end.LatestEndDate

    this.duration <== end.EarliestEndDate - start.EarliestStartDate

    //ParasiticPrecedences = SortedSet.empty[Int]
  }

  override def addDynamicPredecessor(t: Activity) {
    start.addDynamicPredecessor(t)
  }

  override def removeDynamicPredecessor(t:Activity){
    start.removeDynamicPredecessor(t)
  }
  override def getEndActivity: Activity = end.getEndActivity
  override def getStartActivity: Activity = start.getStartActivity

  override def addStaticPredecessor(j: Activity) {
    start.addStaticPredecessor(j)
  }

  override def removeNonTightAdditionalPredecessors(){} //nothing to be done here because no such dependencies can exist

  }

object SuperTask {
  def apply(start: Activity, end: Activity, name: String = "") = new SuperTask(start,end,name)
}

/**
 *
 * @param duration
 * @param planning
 * @param name
 * @param Shifter a function that builds a shifter. A shifter is a function: start,duration => shifted start, that postpones a starting date to avoid some impossibilities
 */
case class Activity(duration: IntVar, planning: Planning, name: String = "", Shifter:(IntVar,IntVar) => IntVar = (a:IntVar,_) => a) {
  val ID: Int = planning.AddActivity(this)

  /**Used for marking algorithm. Must always be set to false between algorithm execution*/
  var Mark:Boolean =  false;

  override def toString(): String = name

  var StaticPredecessors: List[Activity] = List.empty

  def addStaticPredecessor(j: Activity) {
    StaticPredecessors = j :: StaticPredecessors
  }

  def precedes(j: Activity) {
    j.addStaticPredecessor(this)
  }

  def uses(n:IntVar):ActivityAndAmount = ActivityAndAmount(this,n)

  case class ActivityAndAmount(t:Activity, amount:IntVar){
    def ofResource(r:CumulativeResource){t.usesCumulativeResource(r, amount)}

    def ofResources(rr:CumulativeResource*){
      for (r <- rr){t.usesCumulativeResource(r, amount)}
    }
  }

  def removeNonTightAdditionalPredecessors(){
    for(iD:Int <- AdditionalPredecessors.value){
      if(!PotentiallyKilledPredecessors.value.contains(iD)){
        AdditionalPredecessors :-= iD
      }
    }
  }

  var Resources: List[(CumulativeResource, IntVar)] = List.empty

  /**use this method to add resource requirement to a activity.
   * the activity and the resource must be registered to the same planning
   * @param r a resource that the activity uses
   * @param amount the amount of this resource that the activity uses
   */
  def usesCumulativeResource(r: CumulativeResource, amount: IntVar) {
    Resources = (r, amount) :: Resources
    r.notifyUsedBy(this, amount)
  }

  var EarliestStartDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, duration.value, "esd(" + name + ")")
  val EarliestEndDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, duration.value, "eed(" + name + ")")
  EarliestEndDate <== EarliestStartDate + duration

  val LatestEndDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, planning.maxduration, "led(" + name + ")")

  val LatestStartDate: IntVar = LatestEndDate - duration
  var AllSucceedingActivities: IntSetVar = null

  var AdditionalPredecessors: IntSetVar = null
  var AllPrecedingActivities: IntSetVar = null

  var DefiningPredecessors: IntSetVar = null
  var PotentiallyKilledPredecessors: IntSetVar = null

  def addDynamicPredecessor(t: Activity) {
    AdditionalPredecessors :+= t.getEndActivity.ID
  }

  def removeDynamicPredecessor(t:Activity){
    AdditionalPredecessors :-= t.getEndActivity.ID
  }

  def getEndActivity: Activity = this
  def getStartActivity: Activity = this

 // var ParasiticPrecedences:IntSetVar = null
  /**This method is called by the planning when all activities are created*/
  def post() {
    if (AdditionalPredecessors == null){
      AdditionalPredecessors = new IntSetVar(planning.model, 0, planning.Activities.size,
        "added predecessors of " + name, SortedSet.empty)

      val StaticPredecessorsID: SortedSet[Int] = SortedSet.empty[Int] ++ StaticPredecessors.map((j: Activity) => j.ID)
      AllPrecedingActivities = Union(StaticPredecessorsID, AdditionalPredecessors)

      val argMax = ArgMaxArray(planning.EarliestEndDates, AllPrecedingActivities, 0)
      EarliestStartDate <== argMax.getMax

      DefiningPredecessors = argMax

      PotentiallyKilledPredecessors = Inter(DefiningPredecessors, AdditionalPredecessors)

      AllSucceedingActivities = new IntSetVar(planning.model, 0, planning.activityCount - 1, "succeeding_jobs")

      LatestEndDate <== MinArray(planning.LatestStartDates, AllSucceedingActivities, planning.maxduration)

     // ParasiticPrecedences = AdditionalPredecessors minus PotentiallyKilledPredecessors
    }
  }
}

