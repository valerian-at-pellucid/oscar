package oscar.cp.constraints

package stockingCost

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.AllDiffBC
import scala.collection.mutable.PriorityQueue

/**
 * The StockingCost constraint has the following form:
 * $StockingCost([X_1,...,X_n],[d_1,...,d_n],H,c)$ where: 
 *   - the variable $X_i$ is the date of production of item $i$ on the machine,
 *   - the integer $d_i$ is the due-date for item $i$,
 *   - the integer $c$ is the maximum number of items the machine can produce 
 *     during one time slot (capacity), if an item is produced before 
 *     its due date, then it must be stocked. 
 *   - the variable $H$ is an upper bound on the 
 *     total number of slots all the items are need in stock.
 *     
 * This constraint is design for the stocking part of 
 * Production Planning Problem such as Lot Sizing Probem
 *     
 * The StockingCost constraint holds when each item is produced before
 * its due date ($X_i <= d_i$), the capacity of the machine is respected
 * (i.e. no more than $c$ variables $X_i$ have the same value), and $H$
 * is an upper bound on the total stocking cost ($sum_i(d_i - X_i) <= H$).
 * 
 * @author Ratheil Houndji and Pierre Schaus pschaus@gmail.com
 */
class StockingCost(val Y: Array[CPIntVar], val deadline: Array[Int], val H: CPIntVar, val c: Int) extends Constraint(Y(0).store, "StockingCost") {

  val allDiffBC = new AllDiffBC(Y)
  //priorityL2 = 0
  val n = Y.size
  var domMaxMax = Int.MinValue
  var domMinMin = Int.MaxValue
  var k = 0
  while (k < Y.size) {
    val m = Y(k).min
    val M = Y(k).max
    if (m < domMinMin) domMinMin = m
    if (M > domMaxMax) domMaxMax = M
    k += 1
  }
  val X = Array.tabulate(n)(Y(_))
  val Xmax = Array.fill(n+1)(Int.MinValue)
  val d = Array.fill(n)(0)
  val vopt = Array.fill(n)(0)
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    X.foreach(_.callPropagateWhenBoundsChange(this))
    H.callPropagateWhenBoundsChange(this)
    propagate()
  }  
  
  //  --------------map  ---------------------
  var magic = 1
  val mapMagic = Array.fill(domMaxMax-domMinMin+1)(0)
  val map = Array.fill(domMaxMax-domMinMin+1)(0)
 
  def clearMap() {
    magic += 1
  }
  
  def index(k: Int) = k-domMinMin
  
  def insert(k: Int,v: Int) {
    map(index(k)) = v
    mapMagic(index(k)) = magic 
  }
  
  def hasKey(k: Int) = {
    if (k < domMinMin) false
    else mapMagic(index(k)) == magic 
  }
  
  def get(k: Int) = map(index(k))
  
  def printMap () {
	  println("keys:"+(domMinMin to domMaxMax).filter(hasKey(_)).mkString(","))
  }
  
  //  --------------incremental sort  ---------------------

  val sortX = Array.tabulate(X.size)(i => i)
    
  def sortIncremental() {
    var nn = X.size
    var i = 0
    do {
      var newn = 0
      i = 1
      while (i < nn) {
        if (Y(sortX(i - 1)).max < Y(sortX(i)).max) {
          val tmp = sortX(i - 1)
          sortX(i - 1) = sortX(i)
          sortX(i) = tmp
          newn = i
        }
        i += 1
      }
      nn = newn
    } while (nn > 0);
    k = 0;
    while (k < n) {
      X(k) = Y(sortX(k))
      Xmax(k) = X(k).max
      d(k) = deadline(sortX(k))
      k += 1
    }
    
  }  

  override def propagate(): CPOutcome = {
    if (allDiffBC.propagate() == Failure) return Failure
    sortIncremental()
    var t = Xmax(0)
    var i = 0
    var j = 0 //open items {j, ... ,i\} must be placed in some slots 
    var k = 0 //items {k, ... ,i\} have same vOpt
    var u = t+1
    clearMap()
    var Hopt = 0
    var ind = 0
    while (j < i || i < n) {
      while (i < n && Xmax(i) == t) {
        i += 1
      }
      // place at most $c$ items into slot $t$ 
      ind = j
      while(ind <= (i-1).min(j+c-1)){ //update Hopt
        Hopt += d(ind) - t
        ind += 1
      }
      
      if (i - j <= c) { // all the open items can be placed in $t$
        val full = (i-j) == c
        ind = k
        while(ind < i){
          vopt(ind) = t
          ind += 1
        }

        j = i
        k = i
        if (full) {
          insert(t, u)
          if (Xmax(i) < t-1) {
            u = Xmax(i)+1
          }
        } else {
          u = Xmax(i)+1
        }
        t = Xmax(i)
      } else { // all the open items can not be placed in $t$
        insert(t,u) //place $c$ items into slot $t$
        j += c 
        t -= 1
      }
    }
    if (H.updateMin(Hopt) == Failure) return Failure
    val slack = H.max - H.min
    i = 0
    while (i < n) {
      var newmin = vopt(i)  - slack
      
      if (hasKey(newmin)) {
        newmin = vopt(i).min(get(newmin)) 
      }
      X(i).updateMin(newmin)
      i += 1
    }    
    Suspend

  }

}