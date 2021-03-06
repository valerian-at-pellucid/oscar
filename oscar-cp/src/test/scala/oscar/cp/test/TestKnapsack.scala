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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

/**
 * author Pierre Schaus pschaus@gmail.com
 */
class TestKnapsack extends FunSuite with ShouldMatchers  {
  
  def solve(n: Int, cons: Boolean, seed: Int = 0) : Int = { 
	  val rand = new scala.util.Random(seed)
	  val u = 40
	  val profit = Array.fill(n)(1+rand.nextInt(100))
	  val weight = Array.fill(n)(1+rand.nextInt(u))
	  val cp = CPSolver()
	  val P = CPIntVar(0 to 1000000)(cp)
	  val W = CPIntVar(0 to (n/2 * u/2))(cp)
	  val X = Array.fill(profit.size)(new CPBoolVar(cp))
	  var obj = 0
	  //println("W:"+W)
	  //println("weight:"+weight.mkString(","))
	  //println("profit:"+profit.mkString(","))
	  cp.silent = true
	  cp.maximize(P) subjectTo {
	    cp.add(binaryKnapsack(X,profit,P))
	    cp.add(binaryKnapsack(X,weight,W))
	    if (cons) {
	      //println("with knapsack")
	      cp.add(new Knapsack(X,profit,weight,P,W,true))
	    } 
	  } search {
	    if (allBounds(X)) noAlternative
	    else {
	      val (x,i) = X.zipWithIndex.filter{case (x,i) => !x.isBound}.maxBy{case (x,i) => weight(i)}
	      branch(cp.post(x == 1))(cp.post(x == 0))	      
	    }
	  } onSolution {
	    obj = P.value
	  }
	  obj
  }


  test("Knapsack 1") {
    val res1 = solve(5,false,54)
	val res2 = solve(5,true,54)
    res1 should be (res2)
	
    
    for (i <- 0 to 100) {
	 val res1 = solve(20,false,i)
	 val res2 = solve(20,true,i)
	 res1 should be (res2)
	}
	
  }
  
  test("Knapsack 2") {
    val w = Array(25,2,32,36,36)
    val p = Array(76,62,4,91,94)
    val cp = CPSolver()
    val X = Array.fill(w.size)(CPBoolVar()(cp))
    val P = CPIntVar(155 to 170)(cp)
    val W = CPIntVar(0 to 40)(cp)
    cp.add(new Knapsack(X,p,w,P,W))
    cp.add(X(3) == 0)
    X(0).getValue should be (0)
    X(1).getValue should be (1)
    X(2).getValue should be (0)
    X(3).getValue should be (0)
    X(4).getValue should be (1)
    P.getValue should be (156)
    W.getValue should be (38)
    cp.isFailed should be (false)

  }
  
  test("Knapsack 3") {
  
    val p = Array(57,85,71,24,33)
    val w = Array(10,40,37,21,30)
    val cp = CPSolver()
    val X = Array.fill(w.size)(CPBoolVar()(cp))
    val P = CPIntVar(85 to 170)(cp)
    val W = CPIntVar(0 to 40)(cp)
    cp.add(X(1) == 0)
    cp.add(new Knapsack(X,p,w,P,W))
    X(0).getValue should be (1)
    X(1).getValue should be (0)
    X(2).getValue should be (0)
    X(3).getValue should be (0)
    X(4).getValue should be (1)
    P.getValue should be (90)
    W.getValue should be (40)
    cp.isFailed should be (false)

  }  
  

  
  

  
  
  

}
