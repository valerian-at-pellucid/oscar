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
package oscar.cp.constraints.implementations;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * AtLeastNValue Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AtLeastNValue extends Constraint {
	
	CPVarInt [] x;
	CPVarInt  nval;

    /**
     * This is a generalization of the AllDifferent constraint where we can specify
     * with a variable the number of different values. <br>
     * Available propagation strengths  are Weak and Strong
     * @param x
     * @param nval the number of different values in x
     * @see CPPropagStrength
     * @see AllDifferent
     */
	public AtLeastNValue(CPVarInt [] x, CPVarInt nval) {
		super(x[0].s(),"AtLeastNValue");
		this.x = x;
		this.nval = nval;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		
		if(l == CPPropagStrength.Weak) {
			if (s().post(new AtLeastNValueFWC(x,nval)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			if (s().post(new AtLeastNValueAC(x,nval)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		return CPOutcome.Success;
	}

}
