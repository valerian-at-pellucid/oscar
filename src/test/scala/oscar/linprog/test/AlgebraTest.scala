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

package oscar.linprog.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._

/**
 * Test functionality of Algebra related to linear programming and the correct domain creation of variables
 */
class AlgebraTest extends FunSuite with ShouldMatchers with LPModel with MIPModel {

  
	val lp = new LPSolver()
  
	val x1 = new LPVar(lp,"x1",3.5,100)
	val x2 = new LPVar(lp,"x2")
	val x3 = new LPVar(lp,"x3",true)
	val x4 = new LPVar(lp,"x4",false)
	

	val mip = new MIPSolver()
  
	val y1 = new MIPVar(mip,"y1",3.5,100)
	val y2 = new MIPVar(mip,"y2")
	val y3 = new MIPVar(mip,"y3",true)
	val y4 = new MIPVar(mip,"y4",false)	
	
  
  test("normalization, simplification and equality of linear expression") {
	  	val expr0 = -2 * x3 -2 * x1 - x2 + x3 + x1 -1 + 1* x3 // equal -x1 -x2 -1
	  	expr0.equals(-x1-x2-1) should equal (true)
		  
	  	val expr1 = (1 -(expr0 - 1) +2 - 1) // 4 + x1 + x2
	    expr1.equals(4+x1+x2) should equal (true)
		  
	  	val expr2 = -expr1 // -4 - x1 - x2
	    expr2.equals(-4-x1-x2) should equal (true)
	  	expr2.equals(-4-sum(List(x1,x2)))
  }
	
  test("the domains of variables of LP") {
	x1.lb should equal (3.5)
	x1.ub should equal (100)
	x1.unbounded should equal (false)

	x2.lb should equal (0)
	x2.unbounded should equal (false)	
	

	x3.unbounded should equal (true)
    
	x4.lb should equal (0)
	x4.unbounded should equal (false)
  }
  
  test("the domains of variables of MIP") {
	y1.lb should equal (3.5)
	y1.ub should equal (100)
	y1.unbounded should equal (false)

	y2.lb should equal (0)
	y2.unbounded should equal (false)	
	

	y3.unbounded should equal (true)
    
	y4.lb should equal (0)
	y4.unbounded should equal (false)
  } 
  
  test("large sum") {
	class MyVar(val index: Int) extends Var {
	  def name = "MyVar"
	  val ub = 0.0
      val lb = 0.0
      
	}
 
	val x = Array.tabulate(100000)(i => new MyVar(i))
	val mysum = sum(x)
	println("large sum done")

	
  }    
  
  
  
  
  

}

