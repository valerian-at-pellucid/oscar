/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
 ******************************************************************************/
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Difference Constraint x != y
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Diff extends Constraint {

	Constraint diffImpl;

    /**
     * Ask that x and y takes different values (x != y)
     * @param x
     * @param y
     * @see AllDifferent
     */
	public Diff(CPVarInt x, int y) {
		super(x.getStore(),"Diff");
		diffImpl = new DiffVal(x,y);
	}

    /**
     * Ask that x and y takes different values (x != y)
     * @param x
     * @param y
     * @see AllDifferent
     */
	public Diff(CPVarInt x, CPVarInt y) {
		super(x.getStore());
		diffImpl = new DiffVar(x,y);
	}
	
 
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (s.post(diffImpl) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}

}


class DiffVal extends Constraint {

	CPVarInt x;
	int y;
	
	public DiffVal(CPVarInt x, int y) {
		super(x.getStore(),"DiffVal");
		this.x = x;
		this.y = y;
	}
		
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (x.removeValue(y) ==  CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}
	
	

}

class DiffVar extends Constraint {

	CPVarInt x, y;
	
	public DiffVar(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"DiffVar");
		this.x = x;
		this.y = y;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome oc = propagate();
		if(oc != CPOutcome.Success){
			x.callPropagateWhenBind(this);
			y.callPropagateWhenBind(this);
		}
		return oc;
	}
	
	@Override
	protected CPOutcome propagate() {
		if(x.isBound()){
			//System.out.println("x bound to "+x.getValue()+" remove from "+y);
			if(y.removeValue(x.getValue()) == CPOutcome.Failure){
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		if(y.isBound()){
			if(x.removeValue(y.getValue()) == CPOutcome.Failure){
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		return CPOutcome.Suspend;
	}

}
