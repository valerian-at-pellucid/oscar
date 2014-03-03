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
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Absolute value constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Abs extends Constraint {
	
	private CPIntVar x;
	private CPIntVar y;


    /**
     * Build a constraint y = |x|
     * @param x
     * @param y
     */
	public Abs(CPIntVar x, CPIntVar y) {
		super(x.store(),"Abs");
		this.x = x;
		this.y = y;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (y.updateMin(0) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (!x.isBound()) {
			x.callPropagateWhenBoundsChange(this,false);
			x.callValBindWhenBind(this);
		}
		if (!y.isBound()) {
			y.callPropagateWhenBoundsChange(this,false);
			y.callValBindWhenBind(this);
		}
		//we can do more propagation with val remove
		return CPOutcome.Suspend;
	}
	
	

	
	@Override
	public CPOutcome propagate() {
		// y = |x|	
		
		if (x.getMin() >= 0) {
			if (y.updateMin(x.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(x.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		else if (x.getMax() <= 0) {
			if (y.updateMin(-x.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(-x.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(-y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(-y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			int maxabsy = Math.max(Math.abs(x.getMax()), Math.abs(x.getMin()));			
			if (y.updateMax(maxabsy) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(-y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valBind(CPIntVar var) {
		//return CPOutcome.Suspend;
		
		if (x.isBound()) {
			
			if (y.assign(Math.abs(x.getValue())) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
			
		} else { // y is bound
			// y = |x|	
			if(!x.hasValue(-y.getValue())) {
				if (x.assign(y.getValue()) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			else if(!x.hasValue(y.getValue())) {
				if (x.assign(-y.getValue()) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}

			else {
				// x can be (y or -y)
				// remove everything except y and -y from x
				for (int v = x.getMin(); v <= x.getMax(); v++) {
					if(v != y.getValue() && v != -y.getValue()) {
						if (x.removeValue(v) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
					}
				}
			}
			return CPOutcome.Success;
			
		}
	}
}
