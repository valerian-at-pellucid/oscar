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

import oscar.cp.core.*;

/**
 * Global Cardinality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GCC extends Constraint {
	
	private CPIntVar [] x;
	private int minval;
	private int [] low;
	private int [] up;

    /**
     * Constraint the values minval+i to appear between low[i] and up[i] times in x
     * @param x
     * @param minval
     * @param low
     * @param up
     * @see SoftGCC
     * @see GCCVar
     */
	public GCC(CPIntVar [] x,int minval, int [] low, int [] up) {
		super(x[0].store(),"GCC");
		this.x = x;
		this.minval = minval;
		this.low = low;
		this.up = up;
        // super(x,minval,low,up,CPIntVar.apply(x[0].s(),0,0));
    }
	
	public CPOutcome setup(CPPropagStrength l) {
		CPOutcome ok = CPOutcome.Success;
		if (l == CPPropagStrength.Strong || true) { // desactivate constraint of bertrand because it's buggy
			ok = s().post(new SoftGCC(x, minval, low, up, CPIntVar.apply(s(),0,0)));
		} /*else {
			ok = s().post(new GCCFWC(x, minval, low, up));
		}*/
		if (ok == CPOutcome.Failure) return CPOutcome.Failure;
		else return ok;
	}
	

}//end of class GCCVar
