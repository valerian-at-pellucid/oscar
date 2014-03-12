package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.algo.reversible.ReversibleInt
import scala.collection.mutable.HashSet

class TableSTR(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableSTR"){
  
  val position = Array.tabulate(table.length)(i=>i)
  val currentLimit = new ReversibleInt(s,table.length-1)
  
  val arity = X.length
  val variablesIndexes = 0 until X.length
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    idempotent = true
    if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    Suspend
  }
  
  override def propagate(): CPOutcome = {
    println(currentLimit.getValue)
	val gacValues = Array.fill(arity)(HashSet[Int]())
			
	val unboundVariableIndexes = variablesIndexes.filter(i => !X(i).isBound)
	
	var i = 0
	var cpVarIndex = 0
	var unboundCpVarIndex = -1
	var index = -1
	var tau = Array[Int]()
	var isCurrentTupleValid = true
	var tmpPosition = -1
	
	while (i <= currentLimit.getValue) {
	  index = position(i)
	  tau = table(index)
	  
	  //if is validTuple
	  cpVarIndex = 0
	  isCurrentTupleValid = true
	  while (cpVarIndex < arity && isCurrentTupleValid) {
	    if(!X(cpVarIndex).hasValue(tau(cpVarIndex)))
	        isCurrentTupleValid = false
	    cpVarIndex += 1      
	  }
	  
	  if(isCurrentTupleValid) {
	    unboundCpVarIndex = 0
	    while(unboundCpVarIndex < unboundVariableIndexes.length) {
	    	gacValues(unboundVariableIndexes(unboundCpVarIndex)).add(tau(unboundVariableIndexes(unboundCpVarIndex)))
	    	unboundCpVarIndex += 1
	    }
	    i += 1
	  }
	  else { //removeTuple
	    tmpPosition = position(i)
	    position(i) = position(currentLimit.getValue)
	    position(currentLimit.getValue) = tmpPosition
	    currentLimit.setValue(currentLimit.getValue - 1)
	  }
	    
	}
	
	unboundCpVarIndex = 0
	while(unboundCpVarIndex < unboundVariableIndexes.length) {
	  if (gacValues(unboundVariableIndexes(unboundCpVarIndex)).isEmpty)
	    return Failure
	  if(gacValues(unboundVariableIndexes(unboundCpVarIndex)).size != X(unboundVariableIndexes(unboundCpVarIndex)).size) {
	    for(value <- X(unboundVariableIndexes(unboundCpVarIndex)).domainIterator if(!gacValues(unboundVariableIndexes(unboundCpVarIndex)).contains(value))) {
	    	  X(unboundVariableIndexes(unboundCpVarIndex)).removeValue(value)
	    }
	  }
	  unboundCpVarIndex += 1
    }
	 
    Suspend
  }

}