/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.cp.core._
import scala.io.Source
import scala.collection.mutable.Map
import scampi.search.Branching

/** 
 * Cosmos Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object Cosmos extends CPModel {
	def main(args: Array[String]) {
	  
	  val firstLine::restLines = Source.fromFile("data/cosmoshard.txt").getLines.toList
	  val n = firstLine.toInt
	  
	  val orders = restLines.map(_.split(" ").map(_.toInt))
	  val producers = orders.filter(_(0) > 0)
	  val consumers = orders.filter(_(0) < 0)
	  
	  val tmin = orders.map(_(1)).min
	  val tmax = orders.map(_(2)).max
	  
	  // one variable for each order if we take it or not
	  val cp = new CPSolver()
	  val varMap = Map[Array[Int],CPVarBool]() 
	  orders.foreach(o => varMap += (o -> CPVarBool(cp)))
	  
	  // helper functions
	  def overlap(order: Array[Int], t: Int) = t <= order(2) && t >= order(1) 
	  def vars(l: List[Array[Int]], t: Int) = l.filter(overlap(_,t)).map(varMap(_)).toArray
	  def qty(l: List[Array[Int]], t: Int) = l.filter(overlap(_,t)).map(_(0).abs).toArray


	  // one var for each time slot = the quantity exchanged on that slot
	  val varMapQty = Map[Int,CPVarInt]() 
	  for (t <- tmin to tmax) {
	    val prodUB = producers.map(_(0)).sum
	    varMapQty += (t -> CPVarInt(cp, 0 to prodUB))
	  }
	  
	  // total amount of exchanged quantity
	  val obj: CPVarInt = sum(tmin to tmax)(t => varMapQty(t))
	  
	  cp.onSolution {
	    println((tmin to tmax).map(varMapQty(_).getValue).mkString("\t"))
	  }

	  cp.maximize(obj) subjectTo {
	    for (t <- tmin to tmax) {
	    	cp.add(binaryknapsack(vars(producers,t),qty(producers,t),varMapQty(t)), Strong)
	    	cp.add(binaryknapsack(vars(consumers,t),qty(consumers,t),varMapQty(t)), Strong)
	    } 
	  } exploring {
	    val unboundOrders = orders.filter(!varMap(_).isBound)
	    unboundOrders match {
	      case o::_ => 
	        val order = argMax(unboundOrders)(o => o(0)*(o(2)-o(1))).head
	        cp.branchOn(varMap(order) == 1, varMap(order) == 0)
	      case Nil => Branching.noAlternative
	    }
	  }
	  cp.printStats()
	}
}