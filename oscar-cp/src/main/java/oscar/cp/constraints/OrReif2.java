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

import oscar.algo.reversible.ReversibleBool;
import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPBoolVar;
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * Logical Or Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class OrReif2 extends Constraint {
	
	private CPBoolVar [] x;
	private CPBoolVar y;
	
	ReversibleInt nbBound;
	ReversibleBool ytrue;
	


    /**
     * y is true if at least one of the xi's is true, false otherwise
     * @param x
     * @param y
     */
	public OrReif2(CPBoolVar [] x, CPBoolVar y) {
		super(x[0].store(),"Or");
		this.x = x;
		this.y = y;

	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
	    if (x.length == 2) {
	        if (s().post(new BinaryOr(x[0],x[1],y)) == CPOutcome.Failure) return CPOutcome.Failure;
	        else return CPOutcome.Success;
	    }
		nbBound = new ReversibleInt(s(),0); // number of values assigned to false
		ytrue = new ReversibleBool(s(),false);
		for (int i = 0; i < x.length; i++) {
			if (x[i].isTrue()) {
				if (y.assign(1) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				} else {
					return CPOutcome.Success;
				}
			}
		}
		// we know no variables from X are bound to 1
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			} else {
				assert(x[i].isFalse());
				nbBound.incr();
			}
		}
		if (!y.isBound()) {
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			y.callValBindWhenBind(this);
		}
		else {
			if (y.getValue() == 0) {
				for (int i = 0; i < x.length; i++) {
						if (x[i].assign(0) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
				}
				return CPOutcome.Success;
			} else { // y = true
				ytrue.setValue(true);
				if (nbBound.getValue() == x.length-1) { // only one is not bound to false, this one must be set to true
					for (int i = 0; i < x.length; i++) {
						if (!x[i].isBound()) {
							if (x[i].assign(1) == CPOutcome.Failure) {
								return CPOutcome.Failure;
							}
							return CPOutcome.Success;
						}
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}
	
	
	@Override
	public CPOutcome valBindIdx(CPIntVar var, int idx) {
		if (var.getValue() == 1) {
			if (y.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else {
			nbBound.incr();			
			if (nbBound.getValue() == x.length) {
				if (y.assign(0) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else if (nbBound.getValue() == x.length-1 && ytrue.getValue()){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
						return CPOutcome.Success;
					}
				}
				return CPOutcome.Suspend;
			}
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valBind(CPIntVar yvar) {
		if (yvar.getValue() == 0) {
			for (int i = 0; i < x.length; i++) {
					if (x[i].assign(0) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				
			}
			return CPOutcome.Success;
		} else {
			ytrue.setValue(true);
			if (nbBound.getValue() == x.length-1){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						if (x[i].assign(1) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
						return CPOutcome.Success;
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}

}
