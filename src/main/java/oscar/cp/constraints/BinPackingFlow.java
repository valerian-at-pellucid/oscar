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
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.util.ArrayUtils;
import oscar.reversible.ReversibleInt;

/**
 * Redundant Bin-Packing Flow Constraint
 * @author pschaus@gmail.com
 */
public class BinPackingFlow extends Constraint {
	
	private CPVarInt [] x;
	private int [] sizes;
	private CPVarInt [] l;
	private CPVarInt [] c; // cardinalities

	private ReversibleInt [] l_t; // keep track of the current load of each bin
	private ReversibleInt [] c_t; // keep track of the number of items in each bin
	
	private int [] perm ; //permutation of sorted items i.e. s[perm[i]] <= s[perm[i+1]]
	
	public BinPackingFlow(CPVarInt [] x, int [] sizes, CPVarInt [] l, CPVarInt [] c) {
		super(x[0].s(),"BinPackingFlow");
		this.x = x;
		this.sizes = sizes;
		this.l = l;
		this.c = c;
		perm = ArrayUtils.sortPerm(sizes);
		l_t = new ReversibleInt[sizes.length];
		c_t = new ReversibleInt[sizes.length];
		for (int i = 0; i < l_t.length; i++) {
			l_t[i] = new ReversibleInt(s(),0);
			c_t[i] = new ReversibleInt(s(),0);
		}
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength strength) {
		for (CPVarInt var: x) {
			if (var.updateMax(l.length-1) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (var.updateMin(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if(s().post(new GCCVar(x, 0, c), CPPropagStrength.Strong) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		for (int j = 0; j < l.length; j++) {
			l[j].callPropagateWhenBoundsChange(this,false);
		}
		for (int i = 0; i < sizes.length; i++) {
			if (x[i].isBound()) {
				int j = x[i].getValue();
				l_t[j].setValue(l_t[j].getValue() + sizes[i]);
				c_t[j].incr();
			}
			else {
				x[i].callValBindIdxWhenBind(this,i);
				x[i].callPropagateWhenBind(this,false);
			}
		}
		return propagate();
	}
	
	@Override
	public CPOutcome valBindIdx(CPVarInt x, int idx) {
		int j = x.getValue();
		int size = sizes[idx];
		l_t[j].setValue(l_t[j].getValue() + size);
		c_t[j].incr();
	    return CPOutcome.Suspend;
	}
	
	private void printDebug() {
		for (int i = 0; i < x.length; i++) {
			System.out.println("x"+i+"="+x[i]+ "w"+i+"="+sizes[i]);
		}
		for (int j = 0; j < l.length; j++) {
			System.out.println("load"+j+"="+l[j]+" card:"+c[j]+" packedload="+l_t[j]+" packedcard="+c_t[j]);
		}
		
	}
	
	@Override
	public CPOutcome propagate() {
		//printDebug();
		for (int j = 0; j < l.length; j++) {
			//System.out.println("set card bin "+j);
			if (setCardinality(j) == CPOutcome.Failure) {
				//System.out.println("failure set card");
		        return CPOutcome.Failure;
		    }
		}
		return CPOutcome.Suspend;
	}
	
	/**
	 * Adapt the cardinality of bin j
	 * @param j is the bin index
	 * @return Failure if fail detected when adapting cards, or Suspend otherwise
	 */
	private CPOutcome setCardinality(int j) {
	    int minVal = l[j].getMin();
	    int maxVal = l[j].getMax();
	    //how many items do I need at least to reach minVal ?
	    int v = l_t[j].getValue();
	    int i = x.length-1;
	    int nbAdded = 0;
	    while (v < minVal && i >= 0){
	      if (!x[perm[i]].isBound() && x[perm[i]].hasValue(j)) {
	        v += sizes[perm[i]];
	        nbAdded ++;
	      }
	      i--;
	    }
	    if(v < minVal) return CPOutcome.Failure; //not possible to reach the minimum level
	    int nbMin = nbAdded + c_t[j].getValue();
	    //System.out.println("cardmin="+nbMin);
	    if (c[j].updateMin(nbMin) == CPOutcome.Failure){
	      return CPOutcome.Failure;
	    }
	    // how many items can I use at most before reaching maxVal ?
	    v = l_t[j].getValue();
	    i = 0;
	    nbAdded = 0;
	    while (i < x.length && v+sizes[perm[i]] <= maxVal) {
	      if (!x[perm[i]].isBound() && x[perm[i]].hasValue(j)) {
	        v += sizes[perm[i]];
	        nbAdded ++;
	      }
	      i++;
	    }
	    int nbMax = nbAdded + c_t[j].getValue();
	    //System.out.println("cardmax="+nbMax);
	    if (c[j].updateMax(nbMax) == CPOutcome.Failure){
	      return CPOutcome.Failure;
	    }
		return CPOutcome.Suspend;
	}

}
