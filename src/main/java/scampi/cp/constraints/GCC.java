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

import scampi.cp.core.*;

/**
 * Global Cardinality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GCC extends SoftGCC {

    /**
     * Constraint the values minval+i to appear between low[i] and up[i] times in x
     * @param x
     * @param minval
     * @param low
     * @param up
     * @see SoftGCC
     * @see GCCVar
     */
	public GCC(CPVarInt [] x,int minval, int [] low, int [] up) {
         super(x,minval,low,up,new CPVarInt(x[0].getStore(),0,0));
    }
	

}//end of class GCCVar
