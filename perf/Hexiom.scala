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


import oscar.cp.modeling._
import oscar.cp.constraints._
import oscar.cp.core._

import scala.io.Source




/**
 * Hexiom Problem: http://www.kongregate.com/games/Moonkey/hexiom
 * @author Pierre Schaus pschaus@gmail.com
 */
object Hexiom {
  
  def main(args: Array[String]) {
  

		/* Here is an Hexagon of dimension 3 (size of first row).
		 * Each entry is numbered this way:
		 * 
		 *      00  01  02
		 * 
		 *   03  04  05  06
		 * 
		 * 07  08  09  10  11
		 * 
		 *   12  13  14  15
		 *  
		 *     16  17  18
		 *     
		 * A method neighbors (below) allow to retrieve the set of neighbors for an entry
		 * For instance neighbors(7) = {3,8,12} 
		 */
	  
		val lines = Source.fromFile("data/hexiom40.txt").getLines.toArray
		val oneline = (1 until lines.size).map(lines(_)).foldLeft("")((i,j) => i+j)
		val n = lines(0).toInt // number of entries in the firt row of the hexagon
		
		// ------------------- build the neighbor structure ---------------------
		
		// we use a 2D array to represent the hexagon
		// to retrieve easily the neighbors and to pretty-print (but that's not very important)
		val dim = n*2-1+2*n
		val tab = Array.fill(dim,dim)(" ")
		val vals = Array.fill(dim,dim)(-1)
		var start = n
		for (i <- 0 to n-1) {
		  for (j <- 0 to (n-1+i)) {
		    tab(i*2)(start+j*2) = "*"
		    tab(dim-i*2-1-2)(start+j*2) = "*"
		    vals(i*2)(start+j*2) = 0
		    vals(dim-i*2-1-2)(start+j*2) = 0
		  }
		  start -= 1
		}
		tab.foreach(i => println(i.mkString(" ")))
		
		// number of positions on the map
		var k = 0
		// pos(i) is the index in the 2d array of position i
		val pos = for (i <- 0 until dim; j <- 0 until dim; if (vals(i)(j) == 0)) yield {
		  vals(i)(j) = k
		  k += 1
		  (i,j)
		}
		
		// return the neighbors of entry v
		def neighbors(v: Int) = {
		  val (i,j) = pos(v)
		  def inBound1(t: Int) = t >= 0 && t < dim
		  def inBound2(t: (Int,Int)) = inBound1(t._1) && inBound1(t._2)
		  Set((i,j+2),(i,j-2),(i-2,j+1),(i-2,j-1),(i+2,j+1),(i+2,j-1)).filter(inBound2(_)).map(t => vals(t._1)(t._2)).filter(_ >= 0)
		}
		
		// ------------------- CP Model ---------------------
		
		// compute the number of cardinalities of every type of pawns
		val cardinalities = (0 to 6).map(i => oneline.count((i+'0').toChar ==)).toArray :+ oneline.count(('.').toChar ==) 

		                      
		val cp = CPSolver() 
		cp.silent = true
		// used(i) = true iff there is a pawn at this position
		val used =  Array.fill(k)(CPVarBool(cp)) 
		val dummy = 7 // dummy value when no pawn in the neighborhood
		// card(i) = if (used(i)): number of pawns in the neighbors else: dummy 
		val card =  Array.fill(k)(CPVarInt(cp,0 to 7))
		var nbSol = 0
		cp.solve subjectTo {
		  
		  val tuples = (for (i <- 0 to 6) yield (i,0,7)) ++ (for (i <- 0 to 6) yield (i,1,i))
		  println(tuples)
		  for (i <- 0 until k) { 
		    val nbNeighbors = sum(neighbors(i))(used(_))		    
		    cp.add(table(nbNeighbors,used(i),card(i),tuples))
		  }
		  cp.add(gcc(card,0 to 6,cardinalities,cardinalities),Strong)
		} exploration {
		  
		  while (!allBounds(used)) {
			  val x = used.filter(!_.isBound).head
			  cp.branch {cp.post(x == 1)} {cp.post(x == 0)}
		  }
		  nbSol += 1
	    } run(2000)
		println("nbsol:"+nbSol)
		cp.printStats()

	
  }

}
