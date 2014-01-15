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
 * Sum Less Or Equal Constraint: x[0]+x[1]+...+x[n] <= y
 * @author Pierre Schaus pschaus@gmail.com
 */
public class SumLeEq extends Constraint {
	
	private CPIntVar [] x;
	private CPIntVar y;

	public SumLeEq(CPIntVar [] x, CPIntVar y) {
		super(x[0].s(),"SumLeq");
		this.x = x;
		this.y = y;
	}

    /**
     * sum(x) <= y
     * @param x
     * @param y
     */
	public SumLeEq(CPIntVar [] x, int y) {
		this(x,CPIntVar.apply(x[0].s(),y,y));
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) 
				x[i].callPropagateWhenMinChanges(this,false);
		}
		if (!y.isBound())
			y.callPropagateWhenMaxChanges(this,false);
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome propagate() {
		int maxsumx = 0;
		int minsumx = 0;
		for (int i = 0; i < x.length; i++) {
			maxsumx += x[i].getMax();
			minsumx += x[i].getMin();
		}
		
		if (maxsumx <= y.getMin()) {
			return CPOutcome.Success;
		}
		
		if (y.updateMax(maxsumx) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		for (int i = 0; i < x.length; i++) {
			int minsumxi = minsumx - x[i].getMin();
			int maxi = y.getMax() - minsumxi;
			if (x[i].updateMax(maxi) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}
	
	

}
