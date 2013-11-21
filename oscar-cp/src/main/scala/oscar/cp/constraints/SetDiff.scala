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
 * a - b = c
 */
class SetDiff(val a: CPVarSet, val b: CPVarSet, val c: CPVarSet) extends Constraint(a.s, "SetDiff") {

  override def setup(l: CPPropagStrength): CPOutcome = {

    // --------------------------------------------

    // initial filtering   a - b = c

    // for every value in possible a, if not in c and not required in b, remove it from a
    // for every value in required a, if not in b, required in c
    for (v <- a.possibleNotRequiredValues.toSet; if !c.isPossible(v) && !b.isRequired(v)) {
      if (a.excludes(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }
    for (v <- a.requiredValues; if !b.isPossible(v)) {
      if (c.requires(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }    

    // for every value v in required b => remove it from c

    for (v <- b.requiredValues) {
      if (c.excludes(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }

    // for every value in possible c if not in a possible, remove it from c
    // for every value v in required c => must be required in a and excluded from b   
    for (v <- c.possibleNotRequiredValues.toSet; if !a.isPossible(v)) {
      if (c.excludes(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }
    for (v <- c.requiredValues) {
      if (a.requires(v) == CPOutcome.Failure || b.excludes(v) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }
    // -------------------------------------------
    // incremental filtering daemons
    // -------------------------------------------

    if (!a.isBound) a.filterWhenDomainChanges { d => filtera(d) }

    if (!b.isBound) b.filterWhenDomainChanges { d => filterb(d) }

    if (!c.isBound) c.filterWhenDomainChanges { d => filterc(d) }
    
    return CPOutcome.Suspend
  }

  // if value impossible in a, becomes impossible in c

  // if value required in a, if this value is also required in b, it is removed from c possible
  //                         if this value is not possible in b, it is required in c
  //                         if this value is possible but not required in b, nothing to do in c

  //                         if this value is not possible in c, it is required in b
  //                         if this value is required in c, it is not possible in b
  //                         if this value is possible but not required in c, nothing to do in b  
  def filtera(d: DeltaVarSet): CPOutcome = {
    if (d.possibleChanged) {
      for (v <- d.deltaPossible) {
        if (c.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
    }
    if (d.requiredChanged) {
      for (v <- d.deltaRequired) {
        if (b.isRequired(v)) {
          if (c.excludes(v) == CPOutcome.Failure) {
            return CPOutcome.Failure
          }
        }
        if (!b.isPossible(v)) {
          if (c.requires(v) == CPOutcome.Failure) {

            return CPOutcome.Failure
          }
        }
      }
    }
    CPOutcome.Suspend
  }

  // if value impossible in b, if this value is required in a, it becomes required in c
  //                           if this value is required in c, it becomes required in a
  //                           otherwise nothing to do

  // if value required in b, it is removed from c  
  def filterb(d: DeltaVarSet): CPOutcome = {
    if (d.possibleChanged) {
      for (v <- d.deltaPossible) {
        if (a.isRequired(v)) {
          if (c.requires(v) == CPOutcome.Failure) {
            return CPOutcome.Failure
          }
        }
        if (c.isRequired(v)) {
          if (a.requires(v) == CPOutcome.Failure) {
            return CPOutcome.Failure
          }
        }
      }
    }
    if (d.requiredChanged) {
      for (v <- d.deltaRequired) {
        if (c.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
    }
    CPOutcome.Suspend
  }

  // if value impossible in c, if impossible in b, it becomes impossible in a
  //                           if required in a, it is becomes required in b

  // if value required in c, it becomes required in a and impossible in b  
  def filterc(d: DeltaVarSet): CPOutcome = {
    if (d.possibleChanged) {
      for (v <- d.deltaPossible) {
        if (!a.isPossible(v) && a.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }

        if (a.isRequired(v)) {
          if (b.requires(v) == CPOutcome.Failure) {
            return CPOutcome.Failure
          }
        }
      }
    }
    if (d.requiredChanged) {
      for (v <- d.deltaRequired) {
        if (a.requires(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
        if (b.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
    }
    CPOutcome.Suspend
  }
}