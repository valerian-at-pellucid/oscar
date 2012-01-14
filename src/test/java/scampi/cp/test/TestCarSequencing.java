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
import scampi.cp.constraints.BinaryKnapsack;
import scampi.cp.constraints.Diff;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.CPVarBool;
import scampi.cp.core.CPVarInt;
import scampi.cp.core.Store;
import scampi.cp.util.ArrayUtils;
import scampi.cp.constraints.AllDifferent;
import scampi.cp.constraints.GCC;
import scampi.cp.constraints.MulVar;
import scampi.cp.constraints.Sequence;
import scampi.cp.constraints.Sum;
import scampi.cp.constraints.WeightedSum;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.CPVarInt;
import scampi.cp.core.Store;
import scampi.cp.search.*;
import scampi.cp.util.Counter;
import scampi.reversible.SetIndexedArray;
import scampi.search.Search;
import scampi.search.SolutionObserver;

import junit.framework.TestCase;
import junit.framework.TestCase;
import java.util.Arrays;



/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class TestCarSequencing extends TestCase {

    public TestCarSequencing(String name) {
        super(name);
        
    }

    /**
     * Test the number of solutions in the car sequencing problem
     */
    public void testCarSequencing(){
	
    	//   -----------------  data -----------------------
		int nbCars = 10;
		int nbConfigs = 6;
		int nbOptions = 5	;
		int [] lb = new int [] {1,2,1,2,1};
		int [] ub = new int [] {2,3,3,5,5};
		int [] demand = new int [] {1, 1, 2, 2, 2, 2};// demand for each config
		int [][] requires = new int [][]	{{1, 0, 1, 1, 0}, // nbConfigs x nbOptions
											{0, 0, 0, 1, 0},
											{0, 1, 0, 0, 1},
											{0, 1, 0, 1, 0},
											{1, 0, 1, 0, 0},
											{1, 1, 0, 0, 0}};	
		SetIndexedArray [] options = new SetIndexedArray [nbOptions];
		for (int o = 0; o < nbOptions; o++) {
			options[o] = new SetIndexedArray(0, nbConfigs,true);
			for (int c = 0; c < nbConfigs; c++) {
				if (requires[c][o] == 1) {
					options[o].insert(c);
				}
			}
		}
		//   -----------------  model -----------------------
        final Counter nbSol = new Counter();
    	Store cp = new Store();
    	

		CPVarInt [] line = CPVarInt.getArray(cp, nbCars, 0, nbConfigs-1);
		for (int o = 1; o < nbOptions; o++) {
            System.out.println(Arrays.toString(options[o].getSortedVals()));
			cp.add(new Sequence(line, options[o], ub[o], 0, lb[o]));
		}
		cp.add(new GCC(line, 0, new int[nbConfigs], demand));
    	
		//   -----------------  search -----------------------
		Search search = new Search(cp,new NaryFirstFail(line));
		search.addSolutionObserver(new SolutionObserver() {
			public void solutionFound() {
				nbSol.incr();	
			}
		});
		search.solveAll();
        assertEquals(nbSol.getValue(),860);
    }
    

}

