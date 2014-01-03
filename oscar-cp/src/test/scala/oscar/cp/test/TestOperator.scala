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


class TestOperator extends FunSuite with ShouldMatchers  {
  
  
  test("unary operator") { 
	  val cp = CPSolver()
	  val A = CPVarInt(8)(cp)
	  val B = CPVarInt(-10 to 10)(cp)
	  val C = -A
	  C.value should be(-8)
	  cp.post(B == -C)
	  B.value should be(8)
  }  
  
  test("boolean unary operator 1") { 
	  val cp = CPSolver()
	  val A = CPVarBool(true)(cp)
	  val C = -A
	  C.value should be(-1)
  }  

  test("boolean unary operator 2") { 
	  val cp = CPSolver()
	  val A = CPVarBool(true)(cp)
	  val C = !A
	  C.value should be(0)
  }  
  
  test("boolean unary operator 3") { 
	  val cp = CPSolver()
	  val A = CPVarBool(true)(cp)
	  val C = !(!A)
	  C.isTrue should be(true)
  }
  
  test("boolean unary operator 4") { 
	  val cp = CPSolver()
	  val A = CPVarBool()(cp)
	  val B = CPVarBool()(cp)
	  cp.add((!A || B) == (A ==> B))
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(A,B))
	    nbSol += 1
	  } run()
	  nbSol should be(4)
  }
  
  test("boolean unary operator 5") { 
	  val cp = CPSolver()
	  val A = CPVarBool()(cp)
	  val B = CPVarBool()(cp)
	  cp.add(A && B)
	  A.isTrue should be(true)
	  B.isTrue should be(true)
  }   

 


}
