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
public class ElementCstTable extends Constraint {
	
	int [] y;
	CPVarInt x;
	CPVarInt z;

	//y[x] = z
	public ElementCstTable(int [] y, CPVarInt x, CPVarInt z) {
		super(x.getStore(),"ElementCstTable");
		this.y = y;
		this.x = x;
		this.z = z;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		Table table = new Table(new CPVarInt[]{x,z});
		for (int i = 0; i < y.length; i++) {
			table.addTupple(new int[]{i,y[i]});
		}
		if (s.post(table) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		return CPOutcome.Success;
	}
	
	
}
