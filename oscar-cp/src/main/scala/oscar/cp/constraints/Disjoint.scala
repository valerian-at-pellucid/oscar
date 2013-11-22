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
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._

/**
 * Implementation of Disjoint Constraint (two sets must be disjoint)
 * @author Pierre Schaus pschaus@gmail.com
 */
class Disjoint(val x: CPVarSet, val y: CPVarSet) extends Constraint(x.s, "Disjoint") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    
    def filterY(d: DeltaVarSet): CPOutcome = {
      for (v <- d.deltaRequired) {
        if (y.excludes(v) == Failure) return Failure
      }
      Suspend
    }
    
    def filterX(d: DeltaVarSet): CPOutcome = {
      for (v <- d.deltaRequired) {
        if (x.excludes(v) == Failure) return Failure
      }
      Suspend
    }    
    
    x.filterWhenDomainChanges { d =>
    	filterY(d)
    }
    
    y.filterWhenDomainChanges { d =>
    	filterX(d)
    }
    
    for (v <- x.requiredValues) {
      if (y.excludes(v) == Failure) return Failure
    }
    for (v <- y.requiredValues) {
      if (x.excludes(v) == Failure) return Failure
    }
    
	Success	
  }

}


