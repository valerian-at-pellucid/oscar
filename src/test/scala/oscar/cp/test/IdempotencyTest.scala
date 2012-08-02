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

import oscar.cp.core.Constraint
import oscar.cp.core._
import oscar.reversible._
import oscar.cp.core.CPOutcome
import oscar.cp.modeling._

class IdempotencyTest extends FunSuite with ShouldMatchers  {

   
 
    test("test idempotency") {
      
        var nbCallToPropagate = 0
             
    	class MyCons(val X: CPVarInt,idempotent: Boolean) extends Constraint(X.s, "MyCons") {
 
           if (idempotent) setIdempotent()
           override def setup(l: CPPropagStrength): CPOutcome =  {  
             
            X.callPropagateWhenDomainChanges(this)
	        CPOutcome.Suspend	
           }
  
           override def propagate(): CPOutcome = {
             nbCallToPropagate += 1
             X.removeValue(0)
           }
  
        }
      
        val cp = CPSolver()
        val x = CPVarInt(cp,Set(0,1,2,3))
        cp.add(new MyCons(x,false))
        cp.add(x != 3)
        nbCallToPropagate should equal(2)
        
        
        nbCallToPropagate = 0
        val y = CPVarInt(cp,Set(0,1,2,3))
        cp.add(new MyCons(y,true))
        cp.add(y != 3)
        nbCallToPropagate should equal(1)
        
    	
        
    	
    }
    
    

}

