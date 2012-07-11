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

package oscar.cp.constraints;

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.util.ArrayUtils;

/**
 * Element constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Element {

    /**
     * @param x
     * @param ind a variable with a domain defined on 0..x.length-1
     * @return a variable z linked with x and ind by the relation: x[ind] == z
     */
	public static CPVarInt get(CPVarInt [] x, CPVarInt ind) {
		int min = Integer.MAX_VALUE;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < x.length; i++) {
			min = Math.min(min, x[i].getMin());
			max = Math.max(max, x[i].getMax());
		}
		Store cp = ind.getStore();
		CPVarInt res = new CPVarInt(cp,min,max);
		cp.post(new ElementVar(x, ind, res));
		return res;
	}

    /**
     * @param x
     * @param ind a variable with a domain defined on 0..x.length-1
     * @return a variable z linked with x and ind by the relation: x[ind] == z
     */
	public static CPVarInt get(int [] x, CPVarInt ind) {
		int min = ArrayUtils.min(x);
		int max = ArrayUtils.max(x);
		Store cp = ind.getStore();
		CPVarInt z = new CPVarInt(cp,min,max);
		cp.post(new ElementCst(x, ind, z));
		return z;
	}

    /**
     *
     * @param T
     * @param x a variable with a domain defined on 0..T.length-1
     * @param y a variable with a domain defined on 0..T[i].length-1
     * @return a variable z linked with x and ind by the relation: T[x][y] == z
     */
    public static CPVarInt get(int [][] T, CPVarInt x, CPVarInt y) {
        int min = ArrayUtils.min(T);
        int max = ArrayUtils.min(T);
        Store cp = x.getStore();
		CPVarInt z = new CPVarInt(cp,min,max);
		cp.post(new ElementCst2D(T,x,y,z));
		return z;
    }

}