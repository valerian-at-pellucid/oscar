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
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.Constraint;

public class Garded extends Constraint {
	
	private CPVarBool b;
	private Constraint c;
	private boolean onTrue;
	
	/**
	 * Garded constraint: c is posted only when b becomes true (if onTrue) or when b becomes false (if onFalse)
	 * @param b
	 * @param c
	 * @param onTrue 
     * @see  Constraint#when(cp.core.CPVarBool)
	 */
	public Garded(CPVarBool b, Constraint c, boolean onTrue) {
		super(b.s(),"Garded Constraint");
		this.b = b;
		this.c = c;
		this.onTrue = onTrue;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (!b.isBound()) {
			b.callPropagateWhenBind(this);
			return CPOutcome.Suspend;
		} else {
			if ((b.getValue() == 1 && onTrue) || (b.getValue() == 0 && !onTrue)) {
				if (s.post(c) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}	
	}
	
	@Override
	public CPOutcome propagate() {
		if ((b.getValue() == 1 && onTrue) || (b.getValue() == 0 && !onTrue)) {
			if (s.post(c) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Success;
	}

}
