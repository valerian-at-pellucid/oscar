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
package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._

/**
 *  a magic square of order n is an
 *  arrangement of n^2 numbers, usually distinct integers, in a square,
 *  such that n numbers in all rows, all columns, and both diagonals sum
 *  to the same constant. A normal magic square contains the integers
 *  from 1 to n^2.
 */
object MagicSquare {
	


  def main(args: Array[String]): Unit = {  
    
      val n = 4
  
      val Numbers= 1 to n*n
      val Lines = 0 until n
      val Columns = 0 until n
	  
	  val mip = new MIPSolver(LPSolverLib.glpk)
	 
	  val x = Array.tabulate(n,n,n*n) ((l,c,N) => MIPVar(mip,"x"+(l,c,N), 0 to 1))
	  val s = MIPVar(mip,"s",0 to 10000000)
	  
	  mip.maximize(s) subjectTo {
	     /* each cell must be assigned exactly one integer */
		 for(l <- Lines; c <- Columns)
		   mip.add(sum(Numbers)((n) => x(l)(c)(n-1)) == 1)
		  
        /* each integer must be assigned exactly to one cell */
		 for (n <- Numbers)
		   mip.add(sum(Lines,Columns)((l,c) => x(l)(c)(n-1)) == 1)
		   
		 /* the sum in each row must be the magic sum */  
		 for (l <- Lines)
		   mip.add(sum(Columns,Numbers)((c,n) => x(l)(c)(n-1)*(n)) == s)
		   
		 /* the sum in each column must be the magic sum */
		 for (c <- Columns)
		   mip.add(sum(Lines,Numbers)((l,n) => x(l)(c)(n-1)*(n)) == s)
		 
		 /* the sum in the diagonal must be the magic sum */  
		 mip.add(sum(Lines,Numbers)((l,n) => x(l)(l)(n-1)*(n))==s)
		 
		 /* the sum in the co-diagonal must be the magic sum */
		 //mip.add(sum(Lines,Numbers)((l,n) => x(l)(n-l-1)(n-1)*(n))==s) // TODO: fix this constraint
	  }

	  println("objective: "+mip.getObjectiveValue())
	  
	  for(l <- Lines) {
	    println(Columns.map(c => Numbers.filter(n => x(l)(c)(n-1).getValue == 1)).mkString(","))
	  }
	  mip.release()
  }
}
