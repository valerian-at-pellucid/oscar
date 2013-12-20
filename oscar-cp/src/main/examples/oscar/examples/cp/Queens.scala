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
package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._


/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object Queens extends App {
		
      val cp = CPSolver()
      
      val n = 12 //number of queens
      val Queens = 0 until n
      //variables
      val queens = for(i <- Queens) yield CPVarInt(cp,1 to n)
      
      var nbsol = 0
      cp.onSolution {nbsol += 1}
      
      cp.solve subjectTo {
    	  cp.add(allDifferent(queens)/*,Strong*/)
    	  cp.add(allDifferent(for(i <- Queens) yield queens(i) + i)/*,Strong*/)
    	  cp.add(allDifferent(for(i <- Queens) yield queens(i) - i)/*,Strong*/)
      } search {
        selectMin(queens)(x => !x.isBound)(x => x.size) match {
          case None => noAlternative
          case Some(x) => {
            val v = x.min
            branch(cp.add(x == v))(cp.add(x != v))
          }          
        }
        
      }
      val stats = cp.start()
      //print some statistics
      println("#sol:"+nbsol)
      println(stats)
      
	
}
