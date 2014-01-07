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

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class SetCard(val x: CPVarSet, val c: CPVarInt) extends Constraint(x.store, "SetCard") {
  priorityL2 = CPStore.MAXPRIORL2
  override def setup(l: CPPropagStrength): CPOutcome = {
    x.callPropagateWhenDomainChanges(this, false)
    c.callPropagateWhenBoundsChange(this, false)
    propagate()
  }
  
  override def propagate(): CPOutcome = {
    if (c.updateMin(x.requiredSize) == CPOutcome.Failure) return CPOutcome.Failure
    if (c.updateMax(x.possibleSize) == CPOutcome.Failure) return CPOutcome.Failure
    
    if (c.min > x.possibleSize) CPOutcome.Failure
    else if (c.max < x.requiredSize) CPOutcome.Failure
    else if (c.min == x.possibleSize) {
      // every possible must become required
      x.requiresAll()
      c.updateMax(c.min)
    } else if (c.max == x.requiredSize) {
      // removes every possible not required
      x.excludesAll()
      c.updateMin(c.max)
    } else {
      if (c.updateMin(x.requiredSize) == CPOutcome.Failure) return CPOutcome.Failure
      if (c.updateMax(x.possibleSize) == CPOutcome.Failure) return CPOutcome.Failure
      CPOutcome.Suspend
    }
  }
}
