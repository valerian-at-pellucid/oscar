/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.constraints;

import scampi.cp.core.CPOutcome;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.Constraint;
import scampi.cp.core.CPVarBool;
import scampi.cp.core.CPVarInt;
import scampi.reversible.ReversibleBool;
import scampi.reversible.ReversibleInt;

/**
 * Logical Or Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Or extends Constraint {
	
	private CPVarBool [] x;
	private CPVarBool y;
	
	ReversibleInt nbBound;
	ReversibleBool ytrue;
	


    /**
     * y is true if at least one of the xi's is true, false otherwise
     * @param x
     * @param y
     */
	public Or(CPVarBool [] x, CPVarBool y) {
		super(x[0].getStore());
		this.x = x;
		this.y = y;
		nbBound = new ReversibleInt(s);
		nbBound.setValue(0);
		ytrue = new ReversibleBool(s);
		ytrue.setValue(false);
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			} else {
				nbBound.incr();
			}
		}
		if (!y.isBound())
			y.callValBindWhenBind(this);
		else {
			if (y.getValue() == 0) {
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
		}
		return CPOutcome.Suspend;
	}
	
	
	@Override
	protected CPOutcome valBindIdx(CPVarInt var, int idx) {
		nbBound.incr();
		if (var.getValue() == 1) {
			if (y.assign(1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else {
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
				return CPOutcome.Success;
			}
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt yvar) {
		if (yvar.getValue() == 0) {
			for (int i = 0; i < x.length; i++) {
					if (x[i].assign(0) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				
			}
			return CPOutcome.Success;
		} else {
			ytrue.setValue(true);
		}
		return CPOutcome.Suspend;
	}

}
