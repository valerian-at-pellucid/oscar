/**
 * ***********************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.cp.constraints

import oscar.search._
import oscar.cp.core._
import oscar.cp.modeling._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPObjectiveUnitMinimize(objVar: CPVarInt,n: String = "") extends CPObjectiveUnit(objVar,n) {

  best = Int.MaxValue
  
  override def isOptimum = (best == lb)
  
  // constraint methods
  import TightenType._
  override def propagate(): CPOutcome = {
    if (tightenType == NoTighten) return CPOutcome.Suspend
    def delta = if (tightenType == StrongTighten) 1 else 0
    if (objVar.updateMax(best - delta) == CPOutcome.Failure) {
        return CPOutcome.Failure
    }
    CPOutcome.Suspend
  }



}
