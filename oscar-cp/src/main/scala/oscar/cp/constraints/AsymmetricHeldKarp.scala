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
 *******************************************************************************/

package oscar.cp.constraints

import oscar.cp.core.CPSetVar
import oscar.algo.reversible.ReversibleDouble
import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source
import oscar.algo.reversible.ReversibleInt
import oscar.algo.DisjointSets
import CPOutcome._
import scala.collection.mutable.ArrayBuffer

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class AsymetricHeldKarp(succ: Array[CPIntVar], distMatrix: Array[Array[Int]], cost: CPIntVar)  extends ChannelTSP(succ,distMatrix) {
  
    override def setup(l: CPPropagStrength): CPOutcome = {
      if (s.post(new HeldKarp(edgeVar,edges ,cost)) == Failure) return Failure
      super.setup(l)
    }
  
}