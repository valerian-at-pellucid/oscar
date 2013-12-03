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

import oscar.cp.core._
import oscar.cp.modeling._

class TestCPVarInt extends FunSuite with ShouldMatchers {
	
	test("Test1 : Median") {

		val cp = CPSolver()
		
		val a = CPVarInt(cp, Array(0, 1, 2, 3, 4))

		
		a.median should be(2)
		
		a.removeValue(1)
		
		a.median should be(3)
		
		a.removeValue(4)
		
		a.median should be(2)
		
		
	}
	

	
	test("Test is full") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))
	  x.isFull should be(true)
	  cp.add(x!=3)
	  x.isFull should be(true)
	  cp.add(x!=1)
	  x.isFull should be(false)
	  val y = CPVarInt(cp,Set(0,1,3))
	  x.isFull should be(false)
	  
	}
	
	
	test("Iterator1") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))
	  x.toSet should be(Set(0,1,2,3))
	}	
	
	test("Iterator2") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))+1
	  x.toSet should be(Set(1,2,3,4))
	}	
	
  
  	test("Iterator3a") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(1,3))
	  x.toSet should be(Set(1,3))
	}
	

	
	
	test("Iterator3") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(1,3))-1
	  x.toSet should be(Set(0,2))
	}
	
	
	test("Iterator4") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(1,3,5))-1
	  cp.add(x != 2)
	  x.toSet should be(Set(0,4))
	}
	
	test("Iterator5") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,1 to 5) -1
	  
	  x.toSet should be(Set(0,1,2,3,4))
	  cp.add(x != 2)
	  x.toSet should be(Set(0,1,3,4))
	}
	
	test("isBound test - var does get bound") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Array(10, 20, 30))
		a.isBound should be(false)
		cp.add(a == 10)
		a.isBound should be(true)
	}
	
	test("isBound test - var doesn't get bound") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Array(10, 20, 30))
		a.isBound should be(false)
		
		evaluating {
			cp.add(a < 10)
		} should produce [NoSolutionException]
	}
	
  test("min max size methods test") {

    val cp = CPSolver()

    var x = CPVarInt(cp, -2 to 4)
    
    x.min should be(-2)
    x.max should be(4)
    

    for (i <- 0 to 5) {

      cp.pushState()
      cp.add(x != 0)
      cp.add(x != 2)

      x.size should be(5)
      x.min should be(-2)
      x.max should be(4)

      cp.add(x != -1)
      cp.add(x != -2)

      x.size should be(3)
      x.min should be(1)
      x.max should be(4)

      cp.pop()

      x.size should be(7)
      x.min should be(-2)
      x.max should be(4)
    }

  }
  
  test("domain iterator 1") {
    val cp = CPSolver()
    var x = CPVarInt(cp, Set(1,3,5))
  
    val d = x.domainIterator
    println(d.next)
    d.removeValue should be(CPOutcome.Suspend)
    println(d.next)
    d.removeValue should be(CPOutcome.Suspend)
    println(d.next)
    d.removeValue should be(CPOutcome.Failure)
    d.hasNext should be(false)
  }
  
  
  test("domain iterator 2") {
    val cp = CPSolver()
    var x = CPVarInt(cp, Set(1,3,5,11))
    val initVals = x.toSet
    val d = x.domainIterator
    val removed = (for (i <- 1 to x.size-1) yield {
      val v = d.next
      d.removeValue should be(CPOutcome.Suspend)
      v
    }).toSet
    
    
    removed.size should be(x.size-1)
    d.execute()
    x.size should be(1)
    x.isBound should be(true)
    (initVals -- removed).contains(x.value) should be (true)
  }  
  
  test("domain iterator 3") {
    val cp = CPSolver()
    var x = CPVarInt(cp, Set(1,3,5,11,15,17))
    x.removeValue(15)
    val d = x.domainIterator
    val toRemove = Set(3,11,17)
    for (v <- d; if (toRemove.contains(v))) {
      d.removeValue
    }
    d.execute
    x.toSet should be(Set(1,5))
  }    

}
