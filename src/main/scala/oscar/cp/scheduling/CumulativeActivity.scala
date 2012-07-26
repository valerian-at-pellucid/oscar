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

package oscar.cp.scheduling

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class CumulativeActivity(startVar : CPVarInt, durationVar : CPVarInt,  endVar : CPVarInt, machineVar : CPVarInt, resourceVar : CPVarInt) extends Activity(startVar, durationVar) {

    def machine = machineVar
    
    def resource = resourceVar
  
	/**
	 * smallest quantity of resource
	 */
	def minResource() = resource.getMin()
	
	/**
	 * largest quantity of resource
	 */
	def maxResource() = resource.getMax()
	
	override def toString() = "dur:"+dur+ " from ["+est+","+lst+"[ to ["+ect+","+lct+"[ using ["+minResource+","+maxResource+"] on machine(s) "+machine 
}

object CumulativeActivity {
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : CPVarInt, resource : CPVarInt) = {
		
		new CumulativeActivity(start, duration, start.plus(duration), machine , resource)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : Int, resource : CPVarInt) = {
		
		val m = new CPVarInt(start.getStore(), machine, machine)
		new CumulativeActivity(start, duration, start.plus(duration), m, resource)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : CPVarInt, resource : Int) = {
		
		val r = new CPVarInt(start.getStore(), resource, resource)
		new CumulativeActivity(start, duration, start.plus(duration), machine, r)
	}
	
	def apply(start : CPVarInt, duration : CPVarInt, machine : Int, resource : Int) = {
		
		val m = new CPVarInt(start.getStore(), machine, machine)
		val r = new CPVarInt(start.getStore(), resource, resource)
		new CumulativeActivity(start, duration, start.plus(duration), m, r)
	}
	
	def apply(start : CPVarInt, duration : Int, machine : Int, resource : Int) = {
		
		val m = new CPVarInt(start.getStore(), machine, machine)
		val r = new CPVarInt(start.getStore(), resource, resource)
		val d = new CPVarInt(start.getStore(), duration, duration)
		new CumulativeActivity(start, d, start.plus(duration), m, r)
	}
}

class MirrorCumulativeActivity(act : CumulativeActivity) extends CumulativeActivity(act.start, act.dur, act.end, act.machine, act.resource) {

  	override def start(): CPVarInt = throw new UninitializedFieldError("not available") 
	
	override def end(): CPVarInt = throw new UninitializedFieldError("not available") 
  	
	override def est = -act.lct

	override def lst = -act.ect

	override def ect = -act.lst

	override def lct = -act.est
	
	override def adjustStart(v : Int) = end.updateMax(-v)
	
	override def toString() = "mirror of activity:"+act;
}