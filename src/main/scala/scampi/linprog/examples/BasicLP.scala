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


object BasicLP extends LPModel{
	
  def main(args: Array[String]): Unit = {  
      
	  val lp = new LPSolver()
	   
	  val x1 = new LPVar(lp,"x1")
	  val x2 = new LPVar(lp,"x2",0,1000) 
	  val x3 = new LPVar(lp,"x3",0,1000) 
	  val x4 = new LPVar(lp,"x4",0,1000) 
	   
	  lp.minimize(2 * x1 + x2) subjectTo {
	 	  lp.add(3 * x1 + 2 * x2 + 2 * x3 + 1 * x4 <= 4)
	 	  lp.add(4 * x2 + 3 * x3 + 1 * x4 >= 3)
	  }
  
	  println("objective"+lp.getObjectiveValue())
	  
	  lp.release()
	  
  }
  
  


}
