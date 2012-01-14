/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.core;

/**
 * Represents a view on variable applying an offset on it.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPVarIntView extends CPVarInt {

	private CPVarInt v;
	private int b;
	
	/**
	 * Constructor for a view Y that represents v+b
	 */
	public CPVarIntView(CPVarInt v, int b) {
		super(v.getStore());
		this.v = v;
		this.b = b;
	}
	
	@Override
	public Store getStore() {
		return v.getStore();
	}
	
	public boolean isBound() {
		return v.isBound();
	}
	
	public int getSize() {
		return v.getSize();
	}
	
	public boolean isEmpty() {
		return v.isEmpty();
	}	
	
	public boolean hasValue(int val) {
		return v.hasValue(val-b);
	}
	
	public CPOutcome updateMin(int val) {		
		return v.updateMin(val-b);
	}
	
	public CPOutcome assign(int val) {
		return v.assign(val-b);
	}

	public CPOutcome updateMax(int val) {
		return v.updateMax(val-b);
	}
	
	public CPOutcome removeValue(int val) {
		return v.removeValue(val-b);
	}
	
	public int getMin() {
		return v.getMin()+b;
	}
	
	public int getMax() {
		return v.getMax()+b;
	}
	
	public int getValue() {
		return v.getValue()+b;
	}
	
	@Override
	public String toString() {
		return "view +"+b+" on vals "+v.toString();
	}
	
	public void callPropagateWhenBind(Constraint c) {
		v.callPropagateWhenBind(c);
	}
	
	public void callPropagateWhenBoundsChange(Constraint c) {
		v.callPropagateWhenBoundsChange(c);
	}
	
	public void callPropagateWhenMaxChanges(Constraint c) {
		v.callPropagateWhenMaxChanges(c);
	}
	
	public void callPropagateWhenMinChanges(Constraint c) {
		v.callPropagateWhenMinChanges(c);
	}
	
	public void callPropagateWhenDomainChanges(Constraint c) {
		v.callPropagateWhenDomainChanges(c);
	}
	
	public void callValBindWhenBind(Constraint c) {
		v.callValBindWhenBind(c,this,b);
	}
		
	public void callUpdateBoundsWhenBoundsChange(Constraint c) {
		v.callUpdateBoundsWhenBoundsChange(c,this,b);
	}
	
	public void callUpdateMaxWhenMaxChanges(Constraint c) {
		v.callUpdateMaxWhenMaxChanges(c,this,b);
	}
	
	public void callUpdateMinWhenMinChanges(Constraint c) {
		v.callUpdateMinWhenMinChanges(c,this,b);
	}
		
	public void callValRemoveWhenValueIsRemoved(Constraint c) {
		v.callValRemoveWhenValueIsRemoved(c,this,b);
	}
	
	public void callValBindIdxWhenBind(Constraint c, int idx) {
		v.callValBindIdxWhenBind(c,this,idx,b);
	}
		
	public void callUpdateBoundsIdxWhenBoundsChange(Constraint c, int idx) {
		v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx,b);
	}
	
	public void callUpdateMaxIdxWhenMaxChanges(Constraint c, int idx) {
		v.callUpdateMaxIdxWhenMaxChanges(c,this,idx,b);
	}
	
	public void callUpdateMinIdxWhenMinChanges(Constraint c, int idx) {
		v.callUpdateMinIdxWhenMinChanges(c,this,idx,b);
	}
		
	public void callValRemoveIdxWhenValueIsRemoved(Constraint c, int idx) {
		v.callValRemoveIdxWhenValueIsRemoved(c,this,idx,b);
	}

}
