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



package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.CPOutcome._

/**
 * Implementation of Equality constraint for CPVarSet
 * @author Léonard Debroux leonard.debroux@gmail.com
 */
class SetEq(val a: CPVarSet, val b: CPVarSet) extends Constraint(a.s, "SetEq") {
	override def setup(l: CPPropagStrength): CPOutcome = {
	  
	  def filterB(d: DeltaVarSet): CPOutcome = {
	    for (v <- d.deltaRequired) {
	      if(b.requires(v) == Failure) return Failure
	    }
	    for (v <- d.deltaPossible) {
	      if(b.excludes(v) == Failure) return Failure
	    }
	    Suspend
	  }
	  def filterA(d: DeltaVarSet): CPOutcome = {
	    for (v <- d.deltaRequired) {
	      if(a.requires(v) == Failure) return Failure
	    }
	    for (v <- d.deltaPossible) {
	      if(a.excludes(v) == Failure) return Failure
	    }
	    Suspend
	  }
	  
	  a.filterWhenDomainChanges { d =>
	    filterB(d)
	  }
	  b.filterWhenDomainChanges { d =>
	    filterA(d)
	  }
	  
	  for(v <- a.requiredValues) {
	    if(b.requires(v) == Failure) return Failure
	  }
	  for(v <- b.requiredValues) {
	    if(a.requires(v) == Failure) return Failure
	  }
	  
	  for(v <- a.possibleNotRequiredValues) {
	    if(!(b.possibleNotRequiredValues contains v)){
	      if(a.excludes(v) == Failure) return Failure
	    }
	  }
	  for(v <- b.possibleNotRequiredValues) {
	    if(!(a.possibleNotRequiredValues contains v)){
	      if(b.excludes(v) == Failure) return Failure
	    }
	  }
	  
	  Success
	}
}
