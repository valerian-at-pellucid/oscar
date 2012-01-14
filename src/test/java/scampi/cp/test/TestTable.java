/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.test;

import scampi.cp.constraints.*;
import scampi.cp.core.*;
import scampi.cp.util.*;
import scampi.cp.search.*;
import scampi.reversible.*;
import scampi.search.*;

import junit.framework.TestCase;
import java.util.Arrays;
import java.util.Random;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestTable extends TestCase {
	
	private Store s;	
	
    public TestTable(String name) {
        super(name);
        
    }
    	
	 /**
     * setUp() method that initializes common objects
     */
    protected void setUp() throws Exception {
        super.setUp();
        s = new Store();
    }

    /**
     * tearDown() method that cleanup the common objects
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        s = null;
    }
    
    public void testTable1(){  	
    	CPVarInt [] x = new CPVarInt[3];
    	for (int i = 0; i < x.length; i++) {
			x[i] = new CPVarInt(s,1,3);
		}
    	Table table = new Table(x);
    	table.addTupple(new int[] {1,1,1});
    	table.addTupple(new int[] {1,2,3});
    	s.post(table);
    	
    	assertTrue(x[0].isBound());
    	assertTrue(x[0].getValue()==1);
    	assertTrue(!x[2].hasValue(2));
    	
    	s.post(new Diff(x[2],3));
    	
    	assertTrue(s.getStatus()!= CPOutcome.Failure);
    	assertTrue(x[1].getValue()==1);
    	assertTrue(x[2].getValue()==1);

    }
    
    public void testTable2() {
    	Store cp = new Store();
    	CPVarInt x = new CPVarInt(cp,0,4);
    	CPVarInt y = new CPVarInt(cp,0,4);
    	CPVarInt z = new CPVarInt(cp,0,24);

    	Table table = new Table(new CPVarInt[]{x,y,z});
    	for (int i = 0; i < 5; i++) {
    		for (int j = i+1; j < 5; j++) {
    			table.addTupple(new int[]{i,j,i*4+j-1});
    		}
    	}
    	cp.post(table);
		cp.post(new Eq(z,0));

		assertTrue(x.getValue()==0);
		assertTrue(y.getValue()==1);
		assertTrue(z.getValue()==0);
    }

}
