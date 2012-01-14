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

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Garded extends Constraint {
	
	private CPVarBool b;
	private Constraint c;
	
	/**
	 * Garded constraint: c is posted only when b becomes true
	 * @param b
	 * @param c
     * @see  Constraint#when(cp.core.CPVarBool)
	 */
	public Garded(CPVarBool b, Constraint c) {
		super(b.getStore(),"Garded Constraint");
		this.b = b;
		this.c = c;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (!b.isBound()) {
			b.callPropagateWhenBind(this);
			return CPOutcome.Suspend;
		} else {
			if (b.getValue() == 1) {
				if (s.post(c) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}	
	}
	
	@Override
	public CPOutcome propagate() {
		if (b.getValue() == 1) {
			if (s.post(c) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Success;
	}

}
