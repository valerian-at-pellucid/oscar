/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.reversible.ReversibleInt;

/**
 * Redundant Bin-Packing Flow Constraint
 * @author pschaus@gmail.com
 */
class BinPackingFlowExtended(val x: Array[CPVarInt], val sizes: Array[Int], val l: Array[CPVarInt], val c: Array[CPVarInt]) extends Constraint(x(0).s) {

  val perm = ArrayUtils.sortPerm(sizes);
  val l_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val c_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val candidates_t = Array.fill(c.size)(new ReversibleInt(s, 0))
  val permRev = perm.reverse

  override def setup(strength: CPPropagStrength): CPOutcome = {

    if (x.exists(_.updateMax(l.length - 1) == CPOutcome.Failure)
      || x.exists(_.updateMin(0) == CPOutcome.Failure)
      || s.post(new GCCVar(x, 0, c), CPPropagStrength.Strong) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else {
      for (lt <- l)
        lt.callPropagateWhenBoundsChange(this);
      for ((xt, i) <- x.zipWithIndex) {
        if (xt.isBound) {
          val j = xt.getValue
          l_t(j).setValue(l_t(j).value + sizes(i))
          c_t(j).incr
        } else {
          xt.callValBindIdxWhenBind(this, i);
          xt.callPropagateWhenBind(this);
        }
      }
      for (card <- c) {
        card.callPropagateWhenBoundsChange(this);
        //card.callPropagateWhenBind(this);
      }
      for (variable <- x; if (!variable.isBound)) {
        for (bin <- variable) {
          candidates_t(bin).incr
        }
      }
      propagate()
    }
  }
  

  override def valRemoveIdx(x: CPVarInt, idx: Int, value: Int): CPOutcome = {
    candidates_t(value).decr();
    return CPOutcome.Suspend;
  }


  override def valBindIdx(x: CPVarInt, idx: Int): CPOutcome = {
    val j = x.value;
    val wj = sizes(idx);
    l_t(j).setValue(l_t(j).value + wj);
    c_t(j).incr();
    candidates_t(j).decr();
    return CPOutcome.Suspend;
  }

  override def propagate(): CPOutcome = {
    for (j <- 0 until l.size) {
      if (setCardinality(j) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
      
      if (updateLoad(j) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
    }
    return CPOutcome.Suspend;
  }
  
  def bestLoad(j: Int, cardInit: Int, cardToReach: Int, loadInit: Int, permArray: Array[Int]) = {

      var curCard = cardInit
      var curLoad = loadInit
      for (i <- bestCandidatesForBin(j, permArray) if curCard < cardToReach)
      {
        curLoad += sizes(i)
        curCard += 1
      }
      
      curLoad
  }
  
  def updateLoad(j: Int): CPOutcome = {
      // update load min based on card min
      if (l(j).updateMin(bestLoad(j,c_t(j).value,c(j).min,l_t(j).value,perm)) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
      // update load max based on card max
      if (l(j).updateMax(bestLoad(j,c_t(j).value,c(j).max,l_t(j).value,permRev)) == CPOutcome.Failure) {
        return CPOutcome.Failure
      }
      CPOutcome.Suspend
    
  }

  /**
   * Adapt the cardinality of bin j
   * @param j is the bin index
   * @return Failure if fail detected when adapting cards, or Suspend otherwise
   */
  def setCardinality(j: Int): CPOutcome = {
    if (setMinCard(j) == CPOutcome.Failure)
      return CPOutcome.Failure
    if (setMaxCard(j) == CPOutcome.Failure)
      return CPOutcome.Failure
    return CPOutcome.Suspend
  }

  /**
   * compute the maximum cardinality for the bin `bin`
   */
  def setMaxCard(bin: Int): CPOutcome = {
    val (card, load) = getCard(bin, perm, (binLoad, nextItemSize) => binLoad + nextItemSize <= l(bin).getMax.intValue())
    c(bin).updateMax(card)
  }

  /**
   * compute the minimum cardinality for the bin `bin`
   */
  def setMinCard(bin: Int): CPOutcome = {
    val (card, load) = getCard(bin, permRev, (binLoad, nextItemSize) => l(bin).getMin.intValue() > binLoad)
    if (load < l(bin).getMin.intValue())
      return CPOutcome.Failure
    else
      c(bin).updateMin(card)
  }

  /**
   * compute the minimum cardinality for the bin `bin`
   * @param sorted items is the list of items with items used first at the beginning
   * @param continueLoad(binLoad,nextItemSize) a function that return true if the bin should continue to be loaded
   * @return
   */
  
  val candidatesAvailableForBin = Array.fill(c_t.length)(0)
  
  def getCard(bin: Int, sortedItems: Array[Int], continueLoad: (Int, Int) => Boolean): (Int, Int) = {

    var binCompCard = c_t(bin).getValue // the current value of the computation of the cardinality
    var binLoad = l_t(bin).getValue

    for (i<- bestCandidatesForBin(bin,sortedItems) if continueLoad(binLoad, sizes(i))) {
        binLoad += sizes(i)
        binCompCard += 1
    }

    (binCompCard, binLoad)
  }

/**
 * stream of the item that can go into the bin `bin`
 * An item can be refuted if it make impossible if every previous item are packed in `bin` the bin to fill another one.  
 * This is based on the cardinalities of the others bins
 * 
 */
  	def bestCandidatesForBin(bin:Int,sortedItems: Array[Int]) =
  	{
  	    var i = 0; //the next item to try in sortedItems 
		for (b <- 0 until c_t.size) {
		  candidatesAvailableForBin(b) = if (b == bin) 0 else candidates_t(b).value - (c(b).getMin.intValue - c_t(b).getValue)
		}
		
		def nextAcceptableItem() : Stream[Int] = {
		  if (x(i).hasValue(bin) && !x(i).isBound) {
				val refuteItem = (0 until c_t.length).exists(b => b!= bin &&  x(i).hasValue(b) && candidatesAvailableForBin(b) <= 0)
				(refuteItem) match {
				  case (false) => {
				    for (b <- 0 until c_t.size; if x(i).hasValue(b))
						candidatesAvailableForBin(b) -= 1
				    sortedItems(i) #:: nextAcceptableItem
				  }
				  case (true) if (i== sortedItems.length -1) => Stream.empty
				  case _ => nextAcceptableItem
				}
		  } else 
		  	nextAcceptableItem
		}
		
		nextAcceptableItem

  	}
  
  
}
