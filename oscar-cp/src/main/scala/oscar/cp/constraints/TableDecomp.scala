package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

class TableDecomp(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableDecomp"){
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    idempotent = true
    if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    Suspend
  }
  
  override def propagate(): CPOutcome = {
    for(variable<-X;value<-variable) {
      val varIndex = X.indexOf(variable)
      var valueIsSupported = false
      for(tuple<-table if(!valueIsSupported && tuple(varIndex) == value)) {
        var allValueVariableSupported=true
        for(otherVariable<-X if otherVariable != variable) {
          if(allValueVariableSupported && !otherVariable.hasValue(tuple(X.indexOf(otherVariable)))) 
              allValueVariableSupported = false
        }
        valueIsSupported = allValueVariableSupported
      }
      if(!valueIsSupported){
        if(variable.removeValue(value) == Failure)
          return Failure
      }
        
    }
    Suspend
  }
  
}