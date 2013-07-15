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
package oscar.cp.test;


import junit.framework.TestCase;
import java.util.Arrays;

import oscar.cp.constraints.*;
import oscar.cp.core.*;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSum extends TestCase {
	
	private CPStore s;	
	
    public TestSum(String name) {
        super(name);
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new CPStore();
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
    }
    
    public void testSum1() {  	
    	CPVarInt x0 = CPVarInt.apply(s,0,1);
    	CPVarInt x1 = CPVarInt.apply(s,0,2);
    	CPVarInt x2 = CPVarInt.apply(s,0,2);
    	CPVarInt x3 = CPVarInt.apply(s,0,3);
    	
    	CPVarInt [] cumulatedCounters = new CPVarInt[]{x0,x1,x2,x3};
    	int n = cumulatedCounters.length;
    	int len = 2;
    	int min = 1;
    	int max = 2;
    	
    	// 0-1 , 0-2 , 0-2 , 0-3
    	
    	CPVarInt [] nb = new CPVarInt[3];
    	
        for (int i = 0; i <= n-len; i++) {
            CPVarInt nbVal = cumulatedCounters[i+len-1];
            if (i > 0) {
            	nbVal = nbVal.minus(cumulatedCounters[i-1]);
            }
            nb[i] = nbVal;
            
            if (s.post(new LeEq(nbVal,max)) == CPOutcome.Failure) {
                assertFalse(true);
            }
            if (s.post(new GrEq(nbVal,min)) == CPOutcome.Failure) {
                assertFalse(true);
            }
            assertFalse(s.isFailed());
            
        }
        System.out.println(Arrays.toString(cumulatedCounters));
        System.out.println(Arrays.toString(nb));
    }
    

}
