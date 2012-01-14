/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples

import scampi.linprog.modeling._

/* 
 * The knapsack problem is a well-known problem in combinatorial optimization: 
 * Given a set of items, each with a weight and an utility, determine the count of each item 
 * to include in a collection so that the total weight is less than or equal to a given limit
 * and the total utility is as large as possible.
 * @author gme
 */
object KnapSack extends MIPModel {
  
  def main(args: Array[String]) {
       
    // each object is represented as a tuple (weight, utility)
    val objects = Array((100,40), (50,35), (45,18), (20,4), (10,10), (5,2))
    val capacity = 100
    val size = objects.length
    val Objects = 0 until size
    
    // prepare solver
 	val mip = new MIPSolver(LPSolverLib.glpk)
    
    // binary decision variables: we take the object or not
    val x = Array.tabulate(size) (i => new MIPVar(mip, "x"+i, 0 to 1))
    
    // maximize total utility
    mip.maximize(sum(Objects)(o => x(o)*objects(o)._2)) subjectTo {
      
      // given the limited capacity of the pack
      mip.add(sum(Objects)(o => x(o)*objects(o)._1) <= capacity)
      
    }
        
    val selected = Objects.filter(x(_).getValue == 1)  
    var totalWeight = selected.map(objects(_)._1).sum  
        
    println("Status: " + mip.status)
    println("Utility: " + mip.getObjectiveValue())
    println("Total weight: " + totalWeight)
        
    println("Final object collection: "+selected.map(objects(_)).mkString(","))
        
    mip.release()
  }

}
