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
import scampi.cp.core.CPVarInt;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPObjectiveMinimize extends CPObjective{
	
	public CPObjectiveMinimize(CPVarInt objVar) {
		super(objVar);
	}

    @Override
	public CPOutcome setup(CPPropagStrength l) {
        optimum = objVar.getMin();
		return super.setup(l);
	}
		
	@Override
	protected CPOutcome propagate() {
		if (objVar.updateMax(objVal) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	}

	
	public void tighten() throws RuntimeException {
		if ( !objVar.isBound() ) {
			System.out.println(objVar);
			throw new RuntimeException("objective not bound, not possible to tighten");
		}
		objVal = Math.min(objVal-1, objVar.getValue()-1);
		System.out.println("objective tighten to "+(objVal+1));
	}
	
	public void relax() {
		objVal = Integer.MAX_VALUE;
	}

    public boolean isOptimum() {
        return getBound() < getOptimumBound();
    }
	
}
