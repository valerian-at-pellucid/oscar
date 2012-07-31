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

package oscar.reversible.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.search._
import oscar.reversible._
import scala.collection.JavaConversions._

class OrderedSetTest extends FunSuite with ShouldMatchers {

   
 
    test("test 1") {
    			
    	val myset = new OrderedSet(3,10)
    	myset.toSet() should equal((3 to 10).toSet())
    	
    	myset.removeValue(3)
    	myset.toSet() should equal((4 to 10).toSet())
    	myset.hasValue(3) should equal(false)
    	
    	myset.removeValue(10)
    	myset.toSet() should equal((4 to 9).toSet())
    	myset.hasValue(10) should equal(false)
    	myset.hasValue(9) should equal(true)
    	
    	myset.removeValue(5)
    	myset.toSet should equal((4 to 9).toSet - 5)
    	myset.hasValue(5) should equal(false)
    	
    	myset.removeValue(6)
    	myset.removeValue(7)
    	myset.removeValue(8)
    	myset.removeValue(9)
    	
    	myset.toSet should equal(Set(4))
    	myset.getSize should equal(1)
    	
    	println("---------------------------")
    	println(myset.toSet)
    	myset.hasValue(4) should equal(true)
    	myset.removeValue(5)
    	myset.removeValue(4)
    	
    	myset.toSet should equal(Set())
    	myset.getSize should equal(0)
    	myset.hasValue(4) should equal(false)
    	
    	
    }

}

