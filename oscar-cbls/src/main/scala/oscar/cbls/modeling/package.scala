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
package oscar.cbls


import oscar.cbls.invariants.core.computation.{Store, IntVar, SetVar}
import scala.collection.immutable.SortedSet

/**This package proposes an interface to the primitive of the CBLS engine.
 * that is as similar as possible to the one exhibited by the other engines of OScar.
 */
package object modeling extends Constraints
with ClusterInvariants
with ComplexLogicInvariants
with AccessInvariants
with MinMaxInvariants
with NumericInvariants
with SetInvariants {


}

