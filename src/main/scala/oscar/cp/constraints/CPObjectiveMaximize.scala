package oscar.cp.constraints
import oscar.cp.core._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength

class CPObjectiveMaximize(objVars: CPVarInt*) extends CPObjective(objVars:_*) {
  
	for (i <- 0 until bounds.size) {
      bounds(i) = Int.MinValue
      bestObjs(i) = Int.MinValue
    }
	
	private val upperBounds = Array.tabulate(objVars.size)(i => objVars(i).min)
  
  	override def tighten() = {
  	  if (!currentObjective.isBound) {
  	    throw new RuntimeException("objective not bound:" + currentObjective)
  	  }
  	  if (!s.silent) println("objective "+currentObjectiveIdx+" tightened to "+(bound).max(currentObjective.value)+" ub:"+upperBounds(currentObjectiveIdx))
  	  for (i <- 0 until objVars.size) {
  	    bestObjs(i) = objVars(i).value
  	    bounds(i) = bestObjs(i)
  	  }
  	  bestObj = currentObjective.value
  	  bound = bestObj+1
  	  tightenedOnce(currentObjectiveIdx) = true
  	}
	
  	override def reTighten() = {
  	  bound = bestObj+1
  	}  	
	
	override def relax() = {
	  bound = Int.MinValue
	  bestObj = Int.MinValue
	}
	
	override def isOptimum() = {
	  (0 until bounds.size).forall(i => bounds(i) > upperBounds(i))
	}

	override def propagate(): CPOutcome = {
		for(i <- 0 until objVars.size) {
		  if (objVars(i).updateMin(bounds(i)) == CPOutcome.Failure) {
			return CPOutcome.Failure
		  }
		}
		CPOutcome.Suspend;
	}
	
	override def restoreBest() = {
	  bound = bestObj
	}	

}