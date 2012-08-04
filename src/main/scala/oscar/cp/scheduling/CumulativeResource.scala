package oscar.cp.scheduling

import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class CumulativeResource(scheduler : CPScheduler, capa : Int) extends Resource(scheduler) {
	
	protected val activitiesSet : Map[Activity, CumulativeActivity] = Map()
	
	def activities = activitiesSet.values.toArray
	def capacity   = capa
	
	def addProdConsActivity(activity : Activity, height : Int, atEnd : Boolean) { addActivity(activity, ProdConsActivity(activity, id, height, atEnd)) }
	
	def addActivity(activity : Activity, height : Int) { addActivity(activity, CumulativeActivity(activity, id, height)) }
	
	def addActivity(activity : Activity, height : CPVarInt) { addActivity(activity, CumulativeActivity(activity, id, height)) }
	
	def addActivity(activity : Activity, height : Range) { addActivity(activity, CumulativeActivity(activity, id, height)) }
	
	private def addActivity(act : Activity, cum : CumulativeActivity) {
		if (activitiesSet.contains(act)) 
			throw new InvalidParameterException("The activity is already scheduled on this resource.")
		else 
			activitiesSet += (act -> cum)
	}
	
	def heightOf(act : Activity) : CPVarInt = activitiesSet(act).height

	override def setup() {
		scheduler.add(cumulative(activities, id, max = capa))
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object CumulativeResource {
	
	def apply(scheduler : CPScheduler, capa : Int) = new CumulativeResource(scheduler, capa)
}