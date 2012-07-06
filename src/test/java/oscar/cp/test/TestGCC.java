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

package oscar.cp.test;


import junit.framework.TestCase;
import java.util.Arrays;
import java.util.Random;

import oscar.cp.constraints.*;
import oscar.cp.core.*;
import oscar.cp.search.*;
import oscar.cp.util.*;
import oscar.reversible.*;
import oscar.search.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestGCC extends TestCase {
	Random rand; 
	Integer nb = 0;
	
    public TestGCC(String name) {
        super(name);
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        rand = new Random(0);
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        rand = null;
    }
    
    
    public int [][] getRandomDom(int n) {
    	int low[] = new int[n];
    	int up [] = new int[n];
    	
    	for (int i = 0; i < n; i++) {
			low[i] = rand.nextInt(2);
			up[i] = low[i]+rand.nextInt(2);
		}
    	return new int[][]{low,up};
    }
    
    
    private int [][] getRandomOcc(int n) {
    	int low[] = new int[n];
    	int up [] = new int[n];
    	
    	for (int i = 0; i < n; i++) {
			low[i] = rand.nextInt(1);
			up[i] = low[i]+rand.nextInt(3);
		}
    	return new int[][]{low,up};
    }
    
    /**
     * return the number of sol of the constraints
     */
    private int test(int [][]randomDom,int [][]randomOcc,final boolean gccvar) {
    	Store s = new Store();
    	final CPVarInt [] x = new CPVarInt[randomDom[0].length];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,randomDom[0][i],randomDom[1][i]);
		}
    	final CPVarInt [] o = new CPVarInt[randomOcc[0].length];
    	for (int i = 0; i < o.length; i++) {
			o[i] = new CPVarInt(s,randomOcc[0][i],randomOcc[1][i]);
		}
    	
    	
    	nb = 0;
    	if (gccvar) {
    		s.post(new GCCVar(x,-1,o));
    	} else {
    		s.post(new SoftGCC(x,-1,randomOcc[0],randomOcc[1],new CPVarInt(s,0,0)));
    	}
    	if (s.isFailed()) {
    		return -1;
    	}
    	Search search = new Search(s,new Binary(x));
		search.addSolutionObserver(new SolutionObserver(){
			public void solutionFound() {
				if (gccvar) {
					for(CPVarInt occ :o) {
						assertTrue(occ.isBound());
					}
				}
				nb = nb+1;
			}
		});
		search.solveAll();
		return nb;
    }
    
    public void testGCC1(){  	
    	
    	for (int i = 0; i<150; i++) {
    		int [][] randomDom = getRandomDom(3);
        	int [][] randomOcc = getRandomOcc(4);
        	
        	int r1 = test(randomDom, randomOcc,true);
        	int r2 = test(randomDom, randomOcc,false);
        	assertTrue(r1 == r2);
    	}    	
    }
    
    public void testGCC2(){  	
    	
    	Store cp = new Store();
    	
    	CPVarInt [] x = CPVarInt.getArray(cp, 10, 0, 10);
    	CPVarInt [] o = CPVarInt.getArray(cp, 10, 0, 10);
    	for (int i = 0; i < 2; i++) {
    		for (int v = 0; v < 5; v++) {
    			cp.post(new Eq(x[i*5+v], v));
    			
    		}
    	}
    	cp.post(new GCCVar(x, 0, o));
    	assertFalse(cp.isFailed());
    	for (int i = 0; i < o.length; i++) {
    		assertTrue(o[i].isBound());
    		assertTrue(i < 5 ? o[i].getValue() == 2 : o[i].getValue() == 0);
    	}
    }
    
    

}