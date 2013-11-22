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
import oscar.cp.core.CPOutcome._


/**
 * Implementation of Count Constraint:
 *   N variables of X take the values Y
 * @author Pierre Schaus pschaus@gmail.com
 */
class Count(val N: CPVarInt, val X: Array[CPVarInt], val Y: CPVarInt) extends Constraint(N.s, "Among") {

    /*
     * propagate X & Y -> N
     * ------------------
     * countvmin = nb variables in X bound to v
     * countvmax = nb variables in X having v in its dom
     * Nmin = min_{v in D(Y)} countvmin
     * Nmax = max_{v in D(Y)} countvmax
     * 
     * incremental:
     * supportmin = v in D(Y) such that countvmin = Nmin
     * supportmax = v in D(Y) such that countvmax = Nmax
     * 
     * recompute supportmin/max if not in D(Y) or if its value changes
     * in this case, refilter Nmin/Nmax 
     * 
     * 
     * propagate X & N -> Y
     * -------------------
     * a value v does not appear at least Nmin times in D(Xi)'s => remove it from D(Y)
     * a value v is bound to more Nmax times in D(Xi)'s => remove it from D(Y)
     * 
     * propagate Y & N -> X
     * ----------------------
     * if Y is bound to v:
     * when countvmin = Nmax = remove the value v from unbound variables
     * when countvmax = Nmin = assign the value v to all variables having it
     * 
     * Filtering given by the decomposition with reified constraints:
     * When at most Nmin variables have a non empty intersection with Y, those variables must be equal to Y
     */  
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    
    val minY = Y.min
    val maxY = Y.max
    
    val cmin = Array.tabulate(maxY-minY+1)(v => new ReversibleInt(s,X.count(_.isBoundTo(v+minY))))
    val cmax = Array.tabulate(maxY-minY+1)(v => new ReversibleInt(s,X.count(_.hasValue(v+minY))))
    val supportmin = new ReversibleInt(s,0)
    val supportmax = new ReversibleInt(s,0)
    def countmin(v: Int) = cmin(v-minY)
    def countmax(v: Int) = cmax(v-minY)
    
    var updateSupportMinRequired = true
    def updateSupportMin(): Int = {
      var currMin = Int.MaxValue
      for (v <- Y) {
        if (countmin(v).value < currMin) {
          currMin = countmin(v).value
          supportmin.value = v
        }
      }
      updateSupportMinRequired = false
      currMin
    }
    
    
    var updateSupportMaxRequired = true
    def updateSupportMax(): Int = {
      var currMax = Int.MinValue
      for (v <- Y) {
        if (countmax(v).value > currMax) {
          currMax = countmax(v).value
          supportmax.value = v
        }
      }
      updateSupportMaxRequired = false
      currMax
    }
    
    def filterYBound(): CPOutcome = {
      assert(Y.isBound)
      val v = Y.value
      val mincount = X.count(_.isBoundTo(v))
      val maxcount = X.count(_.hasValue(v))
      if (N.updateMin(mincount) == Failure) {
        return Failure
      }
      else if (N.updateMax(maxcount) == Failure) {
        return Failure
      }
      
      if (mincount == N.max) {
        // remove the value v from unbound variables
        for (x <- X; if !x.isBound) {
          x.removeValue(v)
        }
      }
      if (maxcount == N.min) {
        // assign the value v to all variables having it
        for (x <- X; if !x.isBound && x.hasValue(v)) {
          x.assign(v)
        }
      }
      //println("FilterYBound"+X.mkString(",")+" Y:"+Y+" N:"+N+" countmin:"+mincount+" countmax:"+maxcount)
      Suspend
    }

    def updateN(): CPOutcome = {
      if (updateSupportMinRequired && N.updateMin(updateSupportMin()) == Failure) {
        Failure
      }
      else if (updateSupportMaxRequired && N.updateMax(updateSupportMax()) == Failure) {
        Failure
      }
      else Suspend
    }
        
    def updateLostValue(v: Int): CPOutcome = {
      if (Y.hasValue(v)) {
        if (supportmax.value == v) {
          updateSupportMaxRequired = true
        }
        countmax(v).decr
        if (countmax(v).value < N.min) {
          return Y.removeValue(v)
        }
      }
      Suspend
    }
    
    def updateBindValue(v: Int): CPOutcome = {
      if (Y.hasValue(v)) {
        if (supportmin.value == v) {
          updateSupportMinRequired = true
        }
        countmin(v).incr()
        if (countmin(v).value > N.max) {
          return Y.removeValue(v)
        }
      }
      Suspend
    }
    
    Y.filterWhenDomainChanges { d: DeltaVarInt =>
      // should test in constant time
      if (!Y.hasValue(supportmax.value)) {
        updateSupportMaxRequired = true
      }
      if (!Y.hasValue(supportmin.value)) {
        updateSupportMinRequired = true
      }
      if (updateN() == Failure) Failure
      else if (N.isBound) Success
      else Suspend
    }
    
    Y.filterWhenBind {d =>
    	filterYBound()
    }
    
    def filterX(x: CPVarInt, d: DeltaVarInt): CPOutcome = {
        //println("FilterX"+X.mkString(",")+" Y:"+Y)
        for (v <- d.values) {
          //println("lost value"+v)
          if (updateLostValue(v) == Failure) return Failure
        }
        if (x.isBound) {
          //println("is now bound")
          updateBindValue(x.value)
        }
        if (updateN() == Failure) Failure
        else if (Y.isBound) filterYBound()
        else if (N.isBound) Success
        else Suspend
            
    }
    
    for (x <- X; if !x.isBound) {
      x.filterWhenDomainChanges {d: DeltaVarInt =>
        filterX(x,d)
      }

    }
    
    if (updateN() == Failure) return Failure
    if (N.isBound) return Success
    if (Y.isBound) return filterYBound()
    if (s.post(new Sum(X.map((_ === Y)), N)) == Failure) return Failure
    Success
  }

}


