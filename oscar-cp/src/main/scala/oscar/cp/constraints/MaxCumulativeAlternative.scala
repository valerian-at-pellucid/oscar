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
 * *****************************************************************************/
package oscar.cp.constraints

import scala.math.max
import scala.math.min
import oscar.cp.core.CPStore
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPSolver
import oscar.algo.SortUtils.stableSort

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class MaxCumulativeAlternative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) extends Constraint(starts.head.store, "MaxSweepCumulative") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    // at this point the propagation strength does not influence yet
    if (s.post(new SweepMaxCumulative(starts,durations,ends,demands,resources,capacity,id)) == Failure) {
       return Failure
     }
     if (s.post(new MaxCumulativeCapaCheck(starts,durations,ends,demands,resources,capacity,id)) == Failure) {
       return Failure
     }
    return Success
  }
  

}
  

