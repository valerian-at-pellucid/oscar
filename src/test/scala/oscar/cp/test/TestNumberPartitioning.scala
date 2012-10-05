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

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._

import collection.immutable.SortedSet


import org.scalacheck._

/**
 * Problem 49 of CSPLib <br>
 * find a partition of numbers 1..N into two sets A and B such that: <br>
 * a) A and B have the same cardinality  <br>
 * b) sum of numbers in A = sum of numbers in B  <br>
 * c) sum of squares of numbers in A = sum of squares of numbers in B
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestNumberPartitioning extends FunSuite with ShouldMatchers  {


  test("Number Partitioning") {
    

    
    
    def nbSol(n: Int) = {
           val cp = CPSolver()
 
           
           val x = Array.fill(n)(CPVarBool(cp))
           
           val values = Array.tabulate(n)(i => i+1)
           val values2 = Array.tabulate(n)(i => values(i)*values(i))
           var nbsol = 0
           
           cp.solveAll subjectTo {
             cp.add(x(0) == 1) // break summetries between the two partitions
             cp.add(sum(0 until n)(i => x(i)) == n/2)
             cp.add(sum(0 until n) (i => x(i)*values(i)) == values.sum/2) // sum of numbers in A = sum of numbers in B
             cp.add(sum(0 until n) (i => x(i)*values2(i)) == values2.sum/2) // sum of squares of numbers in A = sum of squares of numbers in B
       
             cp.add(binaryknapsack(x,values,values.sum/2),Weak)
             cp.add(binaryknapsack(x,values2,values2.sum/2),Weak)
             cp.add(binaryknapsack(x,values,values.sum/2),Strong)
             cp.add(binaryknapsack(x,values2,values2.sum/2),Strong)
             
           } exploration {
             cp.binary(x.reverse.map(_.asInstanceOf[CPVarInt]))
             nbsol += 1
           }
           nbsol
       
    }
    
    nbSol(8) should be(1)
    nbSol(10) should be(0)
    nbSol(12) should be(1)
    nbSol(20) should be(24)
        
  }  
  

  


}
