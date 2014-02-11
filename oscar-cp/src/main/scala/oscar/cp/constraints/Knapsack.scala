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
package oscar.cp.constraints


import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome
import scala.collection.JavaConversions._


/**
 * Implementation of Knapsack Constraint.
 * A Knapsack Constraint is the conjunction of two constraints:
 * Sum(i) w(i)*X(i) == W && Sum(i) p(i)*w(i) == P
 * And you generally want to maximize P under weight constraint.
 * Weights must be > 0, Profit must be non negative.
 * @author Pierre Schaus pschaus@gmail.com
 */
class Knapsack(val X: Array[CPBoolVar], val profit: Array[Int], val weight: Array[Int], val P: CPIntVar, val W: CPIntVar, val filter: Boolean = true ) extends Constraint(X(0).store, "Table2") {

  def pre(): Boolean = weight.forall(_ > 0) && profit.forall(_ >= 0)
  
  assert(pre())

  val tol = 10e-5 // tolerance of the alogorithm when dealing with floats
  // sort items by decreasing efficiency (p/w) with tie break on the weights
  val efficiencyPerm = (0 until X.size).sortBy(i => (-profit(i).toDouble/weight(i), -weight(i))).toArray
  val x = efficiencyPerm.map(X(_))
  val p: Array[Double] = efficiencyPerm.map(profit(_).toDouble)
  val w: Array[Int] = efficiencyPerm.map(weight(_))
  val e: Array[Double] = efficiencyPerm.map(i => profit(i).toDouble / weight(i))
  val unbound = new ReversibleOrderedSet(s,0,x.size-1)
  val packedWeight = new ReversibleInt(s,0)
  val packedProfit = new ReversibleInt(s,0)
  
  
  
  override def setup(l: CPPropagStrength): CPOutcome = {    
    if (s.post(new BinaryKnapsack(X, profit, P)) == CPOutcome.Failure) {
      return CPOutcome.Failure;
    }
    if (s.post(new BinaryKnapsack(X,weight,W)) == CPOutcome.Failure) {
    		return CPOutcome.Failure;
    }
    if (!pre) {
     println("Knapasack Constraint Not Posted, weights must be > 0 and profit >= 0")
     return CPOutcome.Success 
    }

    
    if (filter) {
    	x.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
    	for ((y,i) <- x.zipWithIndex) {
    		val ok = if (y.isBound) valBindIdx(y,i) else y.callValBindIdxWhenBind(this,i)
    		if (ok == CPOutcome.Failure) return CPOutcome.Failure
    	}
    	if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    	P.callPropagateWhenMinChanges(this)
    	W.callPropagateWhenMaxChanges(this)
    }
    CPOutcome.Suspend
  }

  
  override def valBindIdx(y: CPIntVar, i: Int) : CPOutcome = {
    unbound.removeValue(i);
    if (y.min == 1) {
    	// add this to the capacity and to the reward
        packedProfit.value = packedProfit.value + p(i).toInt 
        packedWeight.value = packedWeight.value + w(i)  
    }
    return CPOutcome.Suspend
  }
  
  /**
   * @return The index of the critical item, -1 if no such critical items
   */
  def criticalItem() = {
    val i = getCriticalItem()._3
    if (i == -1) {
      i
    } else {
      efficiencyPerm(i)
    }
  }
  
  /**
   * return (profit,weight,s) of already packed items and unbound items up to s-1
   */
  private def getCriticalItem() : (Int,Int,Int) = {
    var profit: Double = packedProfit.value.toDouble
    var weight = packedWeight.value
        
    val ite = unbound.iterator()
    var s = -1 // critical item index
    while (ite.hasNext() && s < 0) {
      val i = ite.next()
      if (weight + w(i) <= W.max) {
        weight += w(i)
        profit += p(i)
      } else {
        // reached the critical item, take a fraction of it to reach max capa
        s = i
      }
    }
    return (profit.toInt,weight,s)
  }
  
 
  override def propagate(): CPOutcome = {
    // try to find the maximum profit under the weight/capa constraint using the linear relaxation
    val (profit: Int,weight: Int,s: Int) = getCriticalItem()
    
    if (s == -1) { // enough capa to take all items
      if (P.updateMax(profit) == CPOutcome.Failure) return CPOutcome.Failure
      else return CPOutcome.Suspend
    } else {
      val weightSlack = W.max - weight
      // fraction of item s taken in the relaxed sol
      val fraq_s = weightSlack.toDouble / w(s) 
      
      val maxProfit = profit + fraq_s * p(s)
      if (P.updateMax(Math.floor(maxProfit).toInt) == CPOutcome.Failure) return CPOutcome.Failure
      // we store the initial weight and profit of the critical item to restore it at the end
      val w_s_init = w(s)
      val p_s_init = p(s)
      
      def restore_s() = {
        w(s) = w_s_init
        p(s) = p_s_init
      }
 
      // try to detect mandatory items between indices 0 and s-1   
      //  +--------------------------+
      //  |      i<s    |s|    j>=s  |
      //  +--------------------------+
      w(s) = w(s) - weightSlack // amount of w_s not used in the relaxed sol
      p(s) -= p(s)*fraq_s
      var gap = P.min - maxProfit 
      var i = unbound.getFirst()
      var j = s
      if (i < s) {
         var w_acc = 0.0
         var p_acc = 0.0
         while (i < s && i != -1 && j!= -1) {
        	 var found = false // become true when the max weight of i that can be removed has been found
        	 while (!found && j != -1) {
        	    val gap = (maxProfit - P.min) - (w_acc * e(i) - p_acc) 
        	    val w_ = gap / (e(i) - e(j))
        	    if (w_ > w(j)) { // must go to next
                  w_acc += w(j) // accumulate the weight
                  p_acc += p(j)
                  j = unbound.getNext(j)
        	    } else { // found the exact weight that fills the gap
                  if (w_acc + w_ + tol < w(i)) { // item i is mandatory
                    val ok = x(i).assign(1) // should not fail
                    assert(ok != CPOutcome.Failure)
                  }
                  found = true
        	    }
        	 }        	 
      		 i = unbound.getNext(i)
      	} 
      }
      
      
      // try to detect forbidden items between indices s+1 and n
      //  +--------------------------+
      //  |      j<=s    |s|    i>s  |
      //  +--------------------------+ 
      w(s) = weightSlack // amount of w_s not used in the relaxed sol
      p(s) = p_s_init *fraq_s
      j = s
      i = unbound.getLast()
      if (i > s) {
         var w_acc = 0.0
         var p_acc = 0.0
         while (i > s && i != -1 && j!= -1) {
           var found = false // become true when the max weight of i that can be removed has been found
            while (!found && j != -1) {
              val gap = (maxProfit - P.min) - (p_acc - w_acc * e(i)) 
              val w_ = gap / (e(j) - e(i))
              if (w_ > w(j)) { // must go to prev
                  w_acc += w(j) // accumulate the weight
                  p_acc += p(j)
                  j = unbound.getPrev(j)
        	  } else { // found the exact weight that fills the gap
                  if (w_acc + w_ + tol < w(i)) { // item i is forbidden because we could force it's complete insertion
                    val ok = x(i).removeValue(1) // should not fail
                    assert(ok != CPOutcome.Failure)
                  }
                  found = true
        	  }
            }
           i = unbound.getPrev(i)
         }
      } 
      restore_s() // restore w(s) and p(s)
      return CPOutcome.Suspend
    }
  }

}


