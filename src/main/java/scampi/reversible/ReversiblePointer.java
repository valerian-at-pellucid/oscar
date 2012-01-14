/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.reversible;

/**
 * Creates a generic reversible pointer
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversiblePointer<T> extends Reversible {
	
	private T pointer;

	public ReversiblePointer(ReversibleSearchNode node) {
		super(node);
	}
	
	public void setValue(T val) {
		assert(val != null);
		if (val != pointer) {
			trail();
			this.pointer = val;
		}
	}

    /**
     * Check if the pointer is different from null
     * @return true if the pointer is != null, false otherwise
     */
	public boolean hasValue() {
		return pointer != null;
	}

    /**
     *
     * @return the current pointer
     */
	public T getValue() {
		return pointer;
	}

	@Override
	protected void addOnTrail() {
		node.getTrail().addEntry(this, pointer);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void restore(Object val) {
		pointer = null;
		pointer = (T) val;
	}
	
	@Override
	public String toString() {
		return pointer+"";
	}

}
