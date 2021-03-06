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
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Implication extends Constraint {

	CPIntVar A;
	CPIntVar B;
	CPIntVar V;


    /**
     * Logical Implication: A => B <=> V
     * @param A
     * @param B
     * @param V
     */
	public Implication(CPBoolVar A, CPBoolVar B, CPBoolVar V) {
		super(A.store(),"Implication");
		this.A = A; 
		this.B = B;
		this.V = V;
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (A.isBound()) {
			if (valBind(A) == CPOutcome.Failure) return CPOutcome.Failure;
		}
		else A.callValBindWhenBind(this);
		
		if (B.isBound()) {
			if (valBind(B) == CPOutcome.Failure) return CPOutcome.Failure;
		}
		else B.callValBindWhenBind(this);
		
		if (V.isBound()) {
			if (valBind(V) == CPOutcome.Failure) return CPOutcome.Failure;
		}
		else V.callValBindWhenBind(this);
		
		return CPOutcome.Suspend;
	}

	
	
	protected int getPriorityBindL1(){
		return CPStore.MAXPRIORL1();
	}


	@Override
	public CPOutcome valBind(CPIntVar var) {
		if (A.isBound()) {
			if (A.isBoundTo(0)) {
				// F => X is always true
				if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// T => B <-> V
				if (B.isBoundTo(0)) { // T => F it means V must be F
					if (V.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				if (B.isBoundTo(1)) { // T => T it means V must be T
					if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				// the case of whether V is bound is treated below
			}
		} 
		if (B.isBound()) {
			if (B.isBoundTo(1)) { // V is always true in this case
				if (V.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// A => F <-> V
				// case A is bound is treated above and V is bound is treated below
			}
		}	
		if (V.isBound()) {
			if (V.getValue() == 0) {
				// only way to get A => B <-> F is to have T => F
				if (A.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				if (B.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
				return CPOutcome.Success;
			} else {
				// V is True
				if (B.isBoundTo(0)) {  // A => F <-> T it means A must be F
					if (A.assign(0) == CPOutcome.Failure) return CPOutcome.Failure;
					return CPOutcome.Success;
				}
				if (B.isBoundTo(1)) { // A => T <-> T it means A can be true of false, doesn't matter
					return CPOutcome.Success;
				}
				if (A.isBoundTo(1)) {
					if (B.assign(1) == CPOutcome.Failure) return CPOutcome.Failure;
				}
			}
		}
		return CPOutcome.Suspend;
	}

}

