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
import oscar.cp.util.ArrayUtils;
import oscar.cp.util.NumberUtils;

/**
 * Multiplication Constraint x * y = z
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulVar extends Constraint {

	private CPVarInt x, y, z;

    /**
     * x * y == z
     * @param x
     * @param y
     * @param z
     * @see CPVarInt#mul(cp.core.CPVarInt)
     */
	public MulVar(CPVarInt x, CPVarInt y, CPVarInt z) {
		super(x.getStore(),"Mul x*y=z");
		this.x = x;
		this.y = y;
		this.z = z;
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		
		if (x == y) {
			if (s.post(new Square(x,z)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		if (z.isBound()) {
			if (z.getValue() == 0 && x.hasValue(0) && y.hasValue(0)) {
				x.callPropagateWhenDomainChanges(this);
				y.callPropagateWhenDomainChanges(this);
			} else {
				x.callPropagateWhenBoundsChange(this);
				y.callPropagateWhenBoundsChange(this);
			}
		} else {
			x.callPropagateWhenBoundsChange(this);
			y.callPropagateWhenBoundsChange(this);
			z.callPropagateWhenBoundsChange(this);
		}
		
		CPOutcome ok = propagate();
		if (ok != CPOutcome.Suspend) {
			return ok;
		}
		
		return CPOutcome.Suspend;
	}
		
	@Override
	protected CPOutcome propagate() {
		if (!z.hasValue(0)) {
			
			if (x.removeValue(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.removeValue(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} 
		if (x.isBound()) { // y * c = z
			if (s.post(new MulCte(y,x.getValue(),z)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (y.isBound()) { // x *c = z
			if (s.post(new MulCte(x,y.getValue(),z)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else if (z.isBound()) { // x * y = c
			if (s.post(new MulCteRes(x,y,z.getValue())) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		} else { // none of the variables are bound

			assert (!x.isBound() && !x.isBound() && !y.isBound());
			// propagation of z (try every combination of x and y's bounds)
			if (z.updateMin(ArrayUtils.min(NumberUtils.safeMul(x.getMin() , y.getMin()),
										   NumberUtils.safeMul(x.getMin() , y.getMax()),
										   NumberUtils.safeMul(x.getMax() , y.getMin()),
									       NumberUtils.safeMul(x.getMax() , y.getMax()))) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (z.updateMax(ArrayUtils.max(NumberUtils.safeMul(x.getMin() , y.getMin()),
										   NumberUtils.safeMul(x.getMin() , y.getMax()),
							               NumberUtils.safeMul(x.getMax() , y.getMin()),
									       NumberUtils.safeMul(x.getMax() , y.getMax()))) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			
			// propagate x
			if (propagateMul(x, y, z) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			// propagate y
			if (propagateMul(y, x, z) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		return CPOutcome.Suspend;
	}
	
	/**
	 * Set min(w) <-- min( ceil(a/c), ceil(a/d), ceil(b/c), ceil(b/d))
	 *     max(w) <-- max( floor(a/c), floor(a/d), floor(b/c), floor(b/d))  
	 * @param w
	 * @param a
	 * @param b
	 * @param c != 0
	 * @param d != 0
	 * @return Suspend if no failure detected during this propagation
	 */
	private CPOutcome propagDiv(CPVarInt w, int a, int b, int c, int d) {
		int wmin = Math.min(NumberUtils.minCeilDiv(a, c, d), NumberUtils.minCeilDiv(b, c, d)); 
		if (w.updateMin(wmin) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		int wmax = Math.max(NumberUtils.maxFloorDiv(a, c, d), NumberUtils.maxFloorDiv(b, c, d)); 
		if (w.updateMax(wmax) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		} 
		return CPOutcome.Suspend;
	}
	
	

	// propagate variable u for expression (u * w = z) with neither of the variable bound
	private CPOutcome propagateMul(CPVarInt u, CPVarInt w, CPVarInt z) {
	   if (w.getMin() > 0 || w.getMax() < 0) { 
		   return propagDiv(u, z.getMin(), z.getMax(), w.getMin(), w.getMax());
	   } else {
	      // w_min < 0 && w_max > 0. 
	      if (z.getMin() <= 0 && z.getMax() >= 0) {
	    	 // cannot filter u because we potentially have u * 0 = 0 
	         return CPOutcome.Suspend;
	      } else {
	    	 assert(!z.isBound());
	    	 int after0 = w.getValueAfter(0);
	    	 int before0 = w.getValueBefore(0);
	    	 if (w.getMin() == 0) {
	    		 return propagDiv(u, z.getMin(), z.getMax(), after0, w.getMax());
	    	 } else if (w.getMax() == 0) {
	    		 return propagDiv(u, z.getMin(), z.getMax(), w.getMin(), before0);
	    	 } else {
	    		 // w_min ... before0 ... 0 ... after0 ... w_max
	    		 int umin = Math.min(NumberUtils.minCeilDiv(z.getMin(), w.getMin(), w.getMax(), before0, after0), 
	    				 			 NumberUtils.minCeilDiv(z.getMax(), w.getMin(), w.getMax(), before0, after0));
	    		 if (u.updateMin(umin) == CPOutcome.Failure) {
	    			 return CPOutcome.Failure;
	    		 }
	    		 int umax = Math.max(NumberUtils.maxFloorDiv(z.getMin(), w.getMin(), w.getMax(), before0, after0), 
	    				 			 NumberUtils.minCeilDiv(z.getMax(), w.getMin(), w.getMax(), before0, after0));
	    		 if (u.updateMax(umax) == CPOutcome.Failure) {
	    			 return CPOutcome.Failure;
	    		 }
	    		 return CPOutcome.Suspend;
	    	 }
	      }
	   }
	}	
}



