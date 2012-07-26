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
package oscar.cp.scheduling;


import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class Activity(startVar: CPVarInt, durVar: CPVarInt) {
    
    private val endVar = startVar.plus(durVar)
	
    def start = startVar
    def end = endVar
    def dur = durVar
    
	def this(startVar: CPVarInt,dur: Int) = this(startVar,new CPVarInt(startVar.getStore(),dur,dur))

	/**
	 * earliest starting time
	 */
	def est() = start.getMin()
	
	
	/**
	 * latest starting time
	 */
	def lst() = start.getMax()
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	def ect() = end.getMin()
	
	/**
	 * latest completion time assuming the smallest duration
	 */
	def lct() = end.getMax()
	
	/**
	 * current minimal duration of this activity
	 */
	def minDuration() = dur.getMin()

	/**
	 * current maximal duration of this activity
	 */
	def maxDuration() = dur.getMax()
	
	
	def adjustStart(v : Int) = start.updateMin(v)	
	
	override def toString() = "dur:"+dur+ " in ["+est+","+lct+"[";

}



class MirrorActivity(val act: Activity)  extends Activity(act.start,act.dur) {

  
	override def start(): CPVarInt = throw new UninitializedFieldError("not available") 
	
	override def end(): CPVarInt = throw new UninitializedFieldError("not available") 
	
	/**
	 * earliest starting time
	 */
	override def est = - act.lct;
	

	/**
	 * latest starting time
	 */
	override def lst = - act.ect;
	

	/**
	 * earliest completion time assuming the smallest duration
	 */
	override def ect = - act.lst
	

	/**
	 * latest completion time assuming the smallest duration
	 */
	override def lct = - act.est
	
	
	override def adjustStart(v : Int) = end.updateMax(-v)

	
	override def toString() = "mirror of activity:"+act;
	
}