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
import oscar.algo.reversible.*;
import oscar.cp.constraints.*;
import oscar.cp.core.*;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestSequence extends TestCase {

	private CPStore s;

    public TestSequence(String name) {
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
    
    public void testSequence1(){
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,0,1);
		}
        SetIndexedArray set = new SetIndexedArray(1,1);
    
        s.add(new EqCons(x[2], 0));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[3].isBound());
        assertTrue(x[1].isBound());
        assertEquals(x[1].getValue(),1);
        assertEquals(x[3].getValue(),1);
        assertFalse(s.isFailed());
    }

    public void testSequence2(){
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(3,3);

        s.add(new EqCons(x[2], 2));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[3].isBound());
        assertTrue(x[1].isBound());
        assertEquals(x[1].getValue(),3);
        assertEquals(x[3].getValue(),3);
        assertFalse(s.isFailed());
    }

    public void testSequence3(){
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(2,3);

        s.add(new EqCons(x[2], 1));
        s.add(new Sequence(x, set, 2, 1, 2));

        assertTrue(x[1].getSize() == 2 && x[1].hasValue(2) && x[1].hasValue(3));
        assertTrue(x[3].getSize() == 2 && x[3].hasValue(2) && x[3].hasValue(3));
        assertFalse(s.isFailed());
    }

    public void testSequence4(){
    	CPIntVar [] x = new CPIntVar[4];
    	for (int i = 0; i < x.length; i++) {
			x[i] = CPIntVar.apply(s,1,5);
		}
        SetIndexedArray set = new SetIndexedArray(4,4);

        s.add(new EqCons(x[2], 1));
        s.post(new Sequence(x, set, 1, 1, 1));

        assertTrue(s.isFailed());
    }
    
    
}
