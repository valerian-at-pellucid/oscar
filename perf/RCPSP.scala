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


import oscar.cp.modeling._
import oscar.search._
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.constraints._
import oscar.cp.core.CPVarInt

/**
 * 
 *  @authors: Pierre Schaus pschaus@gmail.com
 */
object RCPSP {
  
	def main(args: Array[String]) {
       
	    // (duration, consumption)
		val instance = Array((50,1),(30,1),(90,3),(10,2),(20,2),(80,1),(30,2),(20,2),(20,1),(10,1),(10,2),(20,2),(80,1))
	    val capa = 4
	    val horizon = instance.map(_._1).sum
		val Times = 0 to horizon
  
		val cp = CPSolver()

		
		val tasks = instance.map{case(dur,req) => CumulativeActivity(CPVarInt(cp,Times),dur,0,req)}
		val makespan = maximum(tasks.map(_.end))
		cp.minimize(makespan) subjectTo {
		  cp.add(new MaxSweepCumulative(cp,tasks,capa,0))
		} exploration {
		  cp.setTimes(tasks)
		}
		cp.printStats()
	
  }
}