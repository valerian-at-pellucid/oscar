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
import scampi.reversible.ReversibleInt;

/**
 * Table Constraint: allows you to specify the acceptable tuples of a tuple of variables
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Table extends Constraint{

	private CPVarInt [] x;
	private boolean posted = false;
	private TableData data;

	private ReversibleInt [][] support; // for each variable-value the tuple id that supports it

	/**
	 * Create a Table constraint where the possible combinations for x are given by extensions with addTupple(int[]).
	 * @param x
	 * @see  Table#addTupple(int[])
	 */
	public Table(CPVarInt ...x) {
		super(x[0].getStore(),"Table");
		this.x = x;
		data = new TableData(x.length);
	}

	/**
	 *
	 * @param x
	 * @param data represents the valid combinations for x, the arity of x and data must be the same
	 */
	public Table(TableData data, CPVarInt ...x) {
		super(x[0].getStore(),"Table");
		this.x = x;
		if (data.arity != x.length) {
			throw new RuntimeException("Table: arity of data different from arity of x");
		}
		this.data = data;
	}

	/**
	 * Add a valid combination for x
	 * @param tuple
	 * @throws RuntimeException  if the constraint is already posted or if the length of the duple doesn't match the length of x
	 */
	public void addTupple(int ...tuple) throws RuntimeException {
		if(tuple.length != x.length ){
			throw new RuntimeException("lengths of tuple and variables differ "+tuple.length+"!="+x.length);
		}
		if(posted){
			throw new RuntimeException("cannot add a tuple to an already posted table constraint");
		}
		data.addTuple(tuple);
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) throws RuntimeException {

		if(posted){
			throw new RuntimeException("constraint already posted");
		}
		posted = true;
		support = new ReversibleInt[x.length][];
		data.setUp();

		for(int i = 0; i < x.length; i++) {
			if ( pruneVar(i) == CPOutcome.Failure ) {
				return CPOutcome.Failure;
			}
			if( !x[i].isBound() ) {
				x[i].callValBindIdxWhenBind(this,i);
				x[i].callValRemoveIdxWhenValueIsRemoved(this,i);
			}
		}	

		return CPOutcome.Suspend;
	}

	private CPOutcome pruneVar(int i) {
		if (x[i].updateMax(data.getMax(i)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (x[i].updateMin(data.getMin(i)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		support[i] = new ReversibleInt[data.getMax(i)-data.getMin(i)+1];
		for(int v = data.getMin(i); v <= data.getMax(i); v++){
			support[i][v-data.getMin(i)] = new ReversibleInt(s);
			support[i][v-data.getMin(i)].setValue(-1);
		}
		for (Integer v : x[i]) {
			boolean supportFound = false;
			int tu = -1;
			if( data.hasFirstSupport(i, v) ){
				tu = data.getFirstSupport(i, v);
				supportFound = checkTuple(tu);
				while( !supportFound && data.hasNextSupport(i, tu) ) {
					tu = data.getNextSupport(i, tu);
					supportFound = checkTuple(tu);
				}
			}
			if (!supportFound) {
				if( x[i].removeValue(v) == CPOutcome.Failure ){
					return CPOutcome.Failure;
				}
			} else {
				setSupport(i, v, tu);
			}			
		}		
		return CPOutcome.Suspend;
	}


	private boolean checkTuple(int tuple) {
		for(int i=0; i<x.length; i++){
			int v = data.getValue(tuple, i);
			if (!x[i].hasValue(v)) {
				return false;
			}
		}
		return true;
	}

	private int getSupport(int i,int val) {
		return support[i][val-data.getMin(i)].getValue();
	}

	private void setSupport(int i,int val, int tu) {
		support[i][val-data.getMin(i)].setValue(tu);
	}

	/*
	 * x[i] has lost the value tuple[i] so this tuple cannot be a support any more.
	 * It means that any pair var-val using tuple as support must find a new support
	 */
	private CPOutcome updateSupports(int i, int tuple){
		for (int k = 0; k < x.length; k++) {
			if (k != i) { //since the x[i] has lost all the support with values tuple[i], no need to try to find a new one
				int valk = data.getValue(tuple, k);
				if (getSupport(k,valk) == tuple) {
					int tu = tuple;
					boolean supportFound = false;
					while (!supportFound && data.hasNextSupport(k, tu)) {
						tu = data.getNextSupport(k, tu);
						supportFound = checkTuple(tu);
					}
					if (!supportFound) {
						//System.out.println("new suport not found");
						if (x[k].removeValue(valk) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
					} else {
						//System.out.println("new suport found "+tu);
						setSupport(k, valk, tu);
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}

	@Override
	protected CPOutcome valRemoveIdx(CPVarInt var, int idx, int val) {
		//all the support using a tuples with val at index idx are not support any more
		//we iterate on these and try to find new support in case they were used as support
		int tu = getSupport(idx, val);
		do {
			if (updateSupports(idx,tu) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			tu = data.getNextSupport(idx, tu);
		} while (tu >= 0);
		return CPOutcome.Suspend;
	}
	
	
}





