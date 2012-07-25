package oscar.cp.constraints

import oscar.cp.scheduling.EfficientHeap

import scala.collection.mutable.Set
import scala.collection.mutable.Queue

import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.scheduling.MirrorCumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPModel

class NewMaxCumulative(cp: CPSolver, allTasks : Array[CumulativeActivity], limit : Int, r : Int)  extends Constraint(allTasks(0).machine.getStore(), "NewMaxCumulative") {
	
	// The tasks (one array by direction of the sweep)
	val lToRTasks : Array[CumulativeActivity] = allTasks.filter(_.machine.hasValue(r))
	val rToLTasks : Array[CumulativeActivity] = lToRTasks.map(new MirrorCumulativeActivity(_))
	
	val nTasks = lToRTasks.size
	val Tasks  = 0 until nTasks

	// The events to process (min heap on date)
	val hEvents = new EfficientHeap[Event](nTasks*4, (a, b) => a.date < b.date)
	
	// The tasks to check (max heap on resource)
	val hCheck = new EfficientHeap[Tuple2[Int, Int]](nTasks, (a, b) => a._1 > b._1)
	// This array is used to know if a task is in hCheck
	val hCheckContent = new Array[Boolean](nTasks)
	
	// The tasks in conflict with the sweep line (min heap on resource)
	val hConflict = new EfficientHeap[Tuple2[Int, Int]](nTasks, (a, b) => a._1 < b._1)
	// This array is used to know if a task is in hConflict
	val hConflictContent = new Array[Boolean](nTasks)

	val evup = new Array[Boolean](nTasks)
	val mins = new Array[Int](nTasks)
	
	// Sweep line parameters
	val newActiveTasks : Queue[Int] = Queue()
	var delta    = 0
	var deltaBis = 0	
	var gap	     = 0
	
	// Events are preprocessed to speed-up the propagation
	val eventList = Array.tabulate(nTasks){e => new EventList(e)}

	override def setup(l: CPPropagStrength) : CPOutcome = {
		
		// Treat this constraint at the end of the propagation queue
		setPriorityL2(0)
		// Propagate is sufficient to reach the fix point of this constraint
		setIdempotent
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
    		
        		if (!lToRTasks(i).start.isBound) lToRTasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).dur.isBound) lToRTasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).end.isBound) lToRTasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).resource.isBound) lToRTasks(i).resource.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).machine.isBound) lToRTasks(i).machine.callPropagateWhenDomainChanges(this)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
			
		// Left to Right sweep
		if (sweepAlgorithm(lToRTasks) == CPOutcome.Failure) return CPOutcome.Failure
			
		// Right to Left sweep
		if (sweepAlgorithm(rToLTasks) == CPOutcome.Failure) return CPOutcome.Failure
        
		return CPOutcome.Suspend
	}
	
	def initDataStructures(tasks : Array[CumulativeActivity]) {
		
		hEvents.clear
		generateEvents(tasks)
		
		hCheck.clear
		for (i <- Tasks) hCheckContent(i) = false
		
		hConflict.clear
		for (i <- Tasks) hConflictContent(i) = false
			
		newActiveTasks.clear
		
		delta = hEvents.head.date
		deltaBis = hEvents.head.date
		gap = limit
	}
	
	def generateEvents(tasks : Array[CumulativeActivity]) {
		
		for (i <- Tasks; if (tasks(i).machine.isBoundTo(r))) {
			
			if (tasks(i).lst < tasks(i).ect) {
				
				// Those events represent the compulsory part
				hEvents enqueue eventList(i).getSCP(tasks)
				hEvents enqueue eventList(i).getECPD(tasks)}

			if (tasks(i).lst >= tasks(i).ect) {
				
				// Reevaluation of the compulsory part
				hEvents enqueue eventList(i).getCCP(tasks)}
			
			if (tasks(i).est != tasks(i).lst) {
				
				// The task is considered in the pruning process
				hEvents enqueue eventList(i).getPR(tasks)
			}			
		}
	}
	
	def sweepAlgorithm(tasks : Array[CumulativeActivity]) : CPOutcome = {
		
		// INITIALIZATION
		initDataStructures(tasks)
		
		while (!hEvents.isEmpty) {
			
			// HANDLING THE SWEEP-LINE MOVE
			if (delta != deltaBis) {
				
				while (!newActiveTasks.isEmpty) {
				
					val t = newActiveTasks.dequeue
					
					if (tasks(t).minResource > gap) {
						
						hConflict.enqueue((tasks(t).minResource, t))
						hConflictContent(t) = true

					} else if (tasks(t).minDuration > deltaBis - delta) {

						hCheck.enqueue((tasks(t).minResource, t))			
						hCheckContent(t) = true
						mins(t) = delta
						
					} else {
						evup(t) = true
					}
				}
				
				if (!filterMin(tasks, delta, deltaBis)) return CPOutcome.Failure
					
				delta = deltaBis
			}
			
			// HANDLING CURRENT EVENT
			delta = synchronize(tasks, delta)
			
			val event = hEvents.dequeue
			
			if (event.isSCP || event.isECPD) {
				
				gap = gap + event.dec
				
			} else if (event.isPR) {
				
				newActiveTasks.enqueue(event.task)
			}
			
			// GETTING NEXT EVENT
			if (hEvents.isEmpty && !filterMin(tasks, delta, Int.MaxValue)) return CPOutcome.Failure
				
			deltaBis = synchronize(tasks, delta)
		}
		
		return CPOutcome.Suspend
	}
	
	def filterMin(tasks : Array[CumulativeActivity], delta : Int, deltaBis : Int) : Boolean = {
		
		if (gap < 0) 
			return false
		
		// UPDATING TOP TASKS OF hCheck
					
		while (!hCheck.isEmpty && (hEvents.isEmpty || hCheck.head._1 > gap)) {
			
			val check = hCheck.dequeue
			val t = check._2
			val h = check._1					
			hCheckContent(t) = false

			if (delta >= tasks(t).lst || delta - mins(t) >= tasks(t).minDuration || hEvents.isEmpty) {
				
				if (tasks(t).adjustStart(mins(t)) == CPOutcome.Failure) return false
				
				if (!evup(t)) {
					//Update events of the compulsory part of t
					evup(t) = true
				}
			} else {
				hConflict.enqueue(check)
				hConflictContent(t) = true
			}
		}	
		
		// UPDATING TOP TASKS OF hConflict
			
		while(!hConflict.isEmpty && hConflict.head._1 <= gap) {
			
			val conflict = hConflict.dequeue
			val t = conflict._2
			val h = conflict._1
			hConflictContent(t) = false
			
			if (delta >= tasks(t).lst) {
				
				if (tasks(t).adjustStart(tasks(t).lst) == CPOutcome.Failure) return false
				
				if (!evup(t)) {
					//Update events of the compulsory part of t
					evup(t) = true
				}
			} else {
				
				if (deltaBis - delta >= tasks(t).minDuration) {
					
					if (tasks(t).adjustStart(delta) == CPOutcome.Failure) return false
				
					tasks(t).adjustStart(delta)
					
					if (!evup(t)) {
						
						//Update events of the compulsory part of t
						evup(t) = true
					}
				} else {
					
					hCheck.enqueue(conflict)			
					hCheckContent(t) = true
					mins(t) = delta
				}
			}
		}
		
			
		true
	}
	
	def synchronize(tasks : Array[CumulativeActivity], delta : Int) : Int = {
		
		var sync = false
		var event : Event = null
		
		// UPDATING TOP EVENTS
		do {
			
			if (hEvents.isEmpty) return -1
				
			sync = true 
			
			event = hEvents.head
			
			// PROCESSING DYNAMIC EVENT
			if (event.isECPD && !evup(event.task)) {
				
				if (hCheckContent(event.task)) {
				
					hEvents.dequeue
					hEvents.enqueue(new Event(event.eType, event.task, mins(event.task) + tasks(event.task).minDuration, event.dec))
					
				} else if (hConflictContent(event.task)) {	
					
					hEvents.dequeue
					hEvents.enqueue(new Event(event.eType, event.task, tasks(event.task).lst + tasks(event.task).minDuration, event.dec))	
				}
				
				evup(event.task) = true
				sync = false
				
			// PROCESSING CONDITIONAL EVENT	
			} else if (event.isCCP && !evup(event.task) && event.date == delta) {
				
				if (hCheckContent(event.task) && mins(event.task) + tasks(event.task).minDuration > delta) {
				
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task) + tasks(event.task).minDuration, tasks(event.task).minResource))
						
				} else if (hConflictContent(event.task)) {	
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, tasks(event.task).lct, tasks(event.task).minResource))	
				}
			
				evup(event.task) = true
				sync = false
			}
			
		} while (!sync)
			
		return event.date
	}

	/** The Event
	 */
	object EventType extends Enumeration {
		
		type EventType = Value
		
		val SCP  = Value("SCP")
		val ECPD = Value("ECPD")
		val CCP  = Value("CCP")
		val PR   = Value("PR")
	}
	
	import EventType._
	
	class Event(e : EventType, t : Int, private var d : Int, private var inc : Int) extends Enumeration {

		def isSCP  = { e == EventType.SCP }
		def isECPD = { e == EventType.ECPD }
		def isCCP  = { e == EventType.CCP }
		def isPR   = { e == EventType.PR }
		
		def date  = d
		def eType = e
		def dec   = inc
		def task  = t
		
		def date_= (x : Int) {d = x}
		def dec_= (x : Int) {inc = x}
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
	}
	
	class EventList(t : Int) {
		
		val SCP  : Event = new Event(EventType.SCP , t, 0, 0)
		val ECPD : Event = new Event(EventType.ECPD, t, 0, 0)
		val CCP  : Event = new Event(EventType.CCP , t, 0, 0)
		val PR   : Event = new Event(EventType.PR  , t, 0, 0)
		
		def getSCP(tasks : Array[CumulativeActivity]) : Event = {
			SCP.date = tasks(t).lst
			SCP.dec  = -tasks(t).minResource
			return SCP
		}
		
		def getECPD(tasks : Array[CumulativeActivity]) : Event = {
			ECPD.date = tasks(t).ect
			ECPD.dec  = tasks(t).minResource
			return ECPD
		}
		
		def getCCP(tasks : Array[CumulativeActivity]) : Event = {
			CCP.date = tasks(t).lst
			return CCP
		}
		
		def getPR(tasks : Array[CumulativeActivity]) : Event = {
			PR.date = tasks(t).est
			return PR
		}
	}

}