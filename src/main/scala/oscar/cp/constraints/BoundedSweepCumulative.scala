package oscar.cp.constraints

import scala.math.max
import scala.math.min

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.modeling.CPSolver

class BoundedSweepCumulative(cp: CPSolver, allTasks : Array[CumulativeActivity], lb : Int, ub : Int, r : Int) extends SweepCumulativeA(cp, allTasks, lb, ub, r, "MaxSweepCumulative") {

	val eventPointSeries = new Array[Event](nTasks*7)
	
	override def generateCheck(i : Int) {
		
		if (tasks(i).maxResource < lb) {
					
			eventPointSeries(nEvents) = eventList(i).sCheck
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eCheck
			nEvents += 1
		}
	}
	
	override def generateProfileBad(i : Int) : Boolean = {
		
		val capaInc = max(0, tasks(i).minResource)
		val consInc = min(0, tasks(i).maxResource)
		
		if (capaInc != 0 || consInc != 0) {
			
			eventPointSeries(nEvents) = eventList(i).sBadProfile(consInc, capaInc)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eBadProfile(consInc, capaInc)  
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def generateProfileGood(i : Int) : Boolean = {
		
		val capaInc = min(0, tasks(i).minResource)
		val consInc = max(0, tasks(i).maxResource)
		
		if (capaInc != 0 || consInc != 0) {
			
			// Generates events		
			eventPointSeries(nEvents) = eventList(i).sGoodProfile(consInc, capaInc)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eGoodProfile(consInc, capaInc)  
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def consistencyCheck : Boolean = ((nCurrentTasks > 0 && consSumHeight < lb) || capaSumHeight > ub) 

	override def mandatoryCheck(t: Int) : Boolean = (nCurrentTasks == 0 || (consSumHeight - consContrib(t)) >= lb) && (capaSumHeight - capaContrib(t) <= ub)		
	
	override def forbidenCheck(t : Int) : Boolean = (consSumHeight - consContrib(t) + tasks(t).maxResource < lb) || 
			   										(capaSumHeight - capaContrib(t) + tasks(t).minResource > ub)
}