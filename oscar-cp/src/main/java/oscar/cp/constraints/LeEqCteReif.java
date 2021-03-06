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
import oscar.cp.core.CPBoolVar;
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.CPStore;

/**
 * Reified Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LeEqCteReif extends Constraint {

	CPIntVar x;
	int v;
	CPBoolVar b;

    /**
     * Constraint x less or equal to v if and only if b is true <br>
     * x <= v <=> b
     * @param x
     * @param v
     * @param b
     */
	public LeEqCteReif(CPIntVar x, int v, CPBoolVar b) {
		super(x.store(),"GrEqCteReif");
		this.x = x;
		this.v = v;
		this.b = b;
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		priorityBindL1_$eq(CPStore.MAXPRIORL1());
		priorityL2_$eq(CPStore.MAXPRIORL2()-1);
		CPOutcome oc = propagate();
		if(oc == CPOutcome.Suspend){
			b.callValBindWhenBind(this);
			x.callPropagateWhenBoundsChange(this,false);
			if (b.isBound()) {
				oc = valBind(b);
			}
		}
		return oc;
	}
	
	@Override
	public CPOutcome propagate() {
		if (x.getMax() <= v) {
			if (b.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (x.getMin() > v) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			return CPOutcome.Suspend;
		}
	}
		
	@Override
	public CPOutcome valBind(CPIntVar var) {
		if (b.getValue() == 0) {
			//x > v
			if (x.updateMin(v+1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			//x <= v
			if (x.updateMax(v) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}				
		}
		return CPOutcome.Success;
	}

}

