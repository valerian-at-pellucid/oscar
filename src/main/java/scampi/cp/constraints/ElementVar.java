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
import scampi.cp.core.CPVarInt;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ElementVar extends Constraint {
	
	CPVarInt [] y;
	CPVarInt x;
	CPVarInt z;

	//y[x] = z
	public ElementVar(CPVarInt [] y, CPVarInt x, CPVarInt z) {
		super(y[0].getStore(),"ElementVar");
		this.y = y;
		this.x = x;
		this.z = z;
	}
	
	//y[x] = z
	public ElementVar(CPVarInt [] y, CPVarInt x, int z) {
		this(y,x,new CPVarInt(x.getStore(), z, z));
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (x.updateMin(0) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (x.updateMax(y.length-1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (propagate() == CPOutcome.Failure) {
		      return CPOutcome.Failure;
		}
		z.callPropagateWhenBoundsChange(this);
		x.callPropagateWhenDomainChanges(this);			
		for (Integer v: x) {
			y[v].callPropagateWhenBoundsChange(this);
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome propagate() {
		   // z = y[x] 

		   int minY = Integer.MAX_VALUE;
		   int maxY = Integer.MIN_VALUE;
		   for (Integer v: x) {
			   minY = Math.min(minY, y[v].getMin());
			   maxY = Math.max(maxY, y[v].getMax());
		   }

		   // update z
		   if (z.updateMin(minY) == CPOutcome.Failure)
		      return CPOutcome.Failure;
		   if (z.updateMax(maxY) == CPOutcome.Failure)
		      return CPOutcome.Failure;
		   
		   // update x
		   for (Integer v: x) {
			   if (y[v].getMin() > z.getMax() || y[v].getMax() < z.getMin()) {
				   if (x.removeValue(v) == CPOutcome.Failure) {
					   return CPOutcome.Failure;
				   }
			   }
		   }
		   
		   if (x.isBound()) { // x is bound
		      CPVarInt yx = y[x.getValue()];
		      if (yx.updateMin(z.getMin()) == CPOutcome.Failure)
		         return CPOutcome.Failure;
		      if (yx.updateMax(z.getMax()) == CPOutcome.Failure)
		         return CPOutcome.Failure;
		   }
		   return CPOutcome.Suspend;
	}

}
