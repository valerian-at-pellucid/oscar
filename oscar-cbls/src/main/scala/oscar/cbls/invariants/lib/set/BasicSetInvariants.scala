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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.invariants.lib.set

import oscar.cbls.invariants.core.computation._
import collection.immutable.SortedSet
import collection.immutable.SortedMap
import oscar.cbls.invariants.core.propagation.Checker;

/**
 * left UNION right
 * @param left is an intvarset
 * @param right is an intvarset
 * @author renaud.delandtsheer@cetic.be
 */
case class Union(left: CBLSSetVar, right: CBLSSetVar) extends SetInvariant {
  assert(left != right)
  var output: CBLSSetVar = null

  def myMax = left.getMaxVal.max(right.getMaxVal)
  def myMin = left.getMinVal.min(right.getMinVal)

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := left.value.union(right.value)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(left == v || right == v)
    output.insertValue(value)
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(left == v || right == v)
    if (v == left) {
      if (!right.value.contains(value)) {
        output.deleteValue(value)
      }
    } else if (v == right) {
      if (!left.value.contains(value)) {
        output.deleteValue(value)
      }
    } else {
      assert(false)
    }
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.intersect(left.value.union(right.value)).size == output.value.size,
      Some("output.value.intersect(left.value.union(right.value)).size == output.value.size"))
  }
}

/**
 * left INTER right
 * @param left is a CBLSSetVar
 * @param right is a CBLSSetVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Inter(left: CBLSSetVar, right: CBLSSetVar) extends SetInvariant {

  var output: CBLSSetVar = null

  def myMax = left.getMaxVal.min(right.getMaxVal)
  def myMin = left.getMinVal.max(right.getMinVal)

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := left.value.intersect(right.value)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    if (v == left) {
      if (right.value.contains(value)) {
        output.insertValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        output.insertValue(value)
      }
    } else {
      assert(false)
    }
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(left == v || right == v)
    output.deleteValue(value)
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.intersect(left.value.intersect(right.value)).size == output.value.size,
      Some("output.value.intersect(left.value.intersect(right.value)).size == output.value.size"))
  }
}

case class SetMap(a: CBLSSetVar, fun: Int=>Int,
               override val myMin:Int = Int.MinValue,
               override val myMax:Int = Int.MaxValue) extends SetInvariant {

  var output: CBLSSetVar = null

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  var outputCount:SortedMap[Int,Int] = SortedMap.empty

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)

    output := SortedSet.empty

    for(v <- a.value){
      val mappedV = fun(v)
      val oldCount = outputCount.getOrElse(mappedV,0)
      if(oldCount == 0){
        output :+= mappedV
      }
      outputCount += ((mappedV, oldCount+1))
    }
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV,0)
    if(oldCount == 0){
      output :+= mappedV
    }
    outputCount += ((mappedV, oldCount+1))
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    val mappedV = fun(value)
    val oldCount = outputCount.getOrElse(mappedV,0)
    if(oldCount == 1){
      output :-= mappedV
    }
    outputCount += ((mappedV, oldCount-1))

  }

  override def checkInternals(c: Checker) {
    c.check(output.value.intersect(a.value.map(fun)).size == output.value.size)
  }
}

/**
 * left MINUS right, the set diff operator
 * @param left is the base set
 * @param right is the set that is removed from left
 * @author renaud.delandtsheer@cetic.be
 * */
case class Diff(left: CBLSSetVar, right: CBLSSetVar) extends SetInvariant {

  var output: CBLSSetVar = null
  def myMax = left.getMaxVal
  def myMin = left.getMinVal

  registerStaticAndDynamicDependency(left)
  registerStaticAndDynamicDependency(right)
  finishInitialization()

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := left.value.diff(right.value)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    if (v == left) {
      if (!right.value.contains(value)) {
        output.insertValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        output.deleteValue(value)
      }
    } else {
      assert(false)
    }
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    if (v == left) {
      if (!right.value.contains(value)) {
        output.deleteValue(value)
      }
    } else if (v == right) {
      if (left.value.contains(value)) {
        output.insertValue(value)
      }
    } else {
      assert(false)
    }
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.intersect(left.value.diff(right.value)).size == output.value.size,
      Some("output.value.intersect(left.value.diff(right.value)).size == output.value.size"))
  }
}

/**
 * #(v) (cardinality)
 * @param v is an IntSetVar, the set of integers to count
 * @author renaud.delandtsheer@cetic.be
 * */
case class Cardinality(v: CBLSSetVar) extends IntInvariant {

  def myMax = v.getMaxVal - v.getMinVal
  def myMin = 0

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  var output: CBLSIntVar = null

  override def setOutputVar(vv: CBLSIntVar) {
    output = vv
    output.setDefiningInvariant(this)
    output := v.value.size
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(v == this.v)
    output :+= 1
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(v == this.v)
    output :-= 1
  }

  override def checkInternals(c: Checker) {
    c.check(output.value == v.value.size, Some("output.value == v.value.size"))
  }
}

/**
 * makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
 * @param on is a set of IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class MakeSet(on: SortedSet[CBLSIntVar]) extends SetInvariant {

  var output: CBLSSetVar = null
  var counts: SortedMap[Int, Int] = on.foldLeft(SortedMap.empty[Int, Int])((acc:SortedMap[Int,Int], intvar:CBLSIntVar) => acc + ((intvar.value, acc.getOrElse(intvar.value, 0) + 1)))

  for (v <- on) registerStaticAndDynamicDependency(v)
  finishInitialization()

  def myMax = Int.MaxValue
  def myMin = Int.MinValue

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output.setValue(SortedSet.empty[Int] ++ counts.keySet)
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    assert(on.contains(v), "MakeSet notified for non interesting var :" + on.toList.exists(_==v) + " " + on.toList)

    assert(OldVal != NewVal)
    if (counts(OldVal) == 1) {
      //on va en supprimer un
      counts = counts - OldVal
      output.deleteValue(OldVal)
    } else {
      //on en supprime pas un
      counts = counts + ((OldVal, counts(OldVal) - 1))
    }
    if (counts.contains(NewVal)) {
      counts = counts + ((NewVal, counts(NewVal) + 1))
    } else {
      counts = counts + ((NewVal, 1))
      output.insertValue(NewVal)
    }
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.size <= on.size,
      Some("output.value.size (" + output.value.size
        + ") <= on.size (" + on.size + ")"))
    for (v <- on) c.check(output.value.contains(v.value),
      Some("output.value.contains(v.value (" + v.value + "))"))

    for (v <- output.value) c.check(on.exists(i => i.value == v),
      Some("on.exists(i => i.value == " + v +")"))

  }
}

/**
 * makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
 * output = if (lb <= ub) [lb; ub] else empty
 *
 * BEWARE: this invariant is not efficient because if you change a bound with a delta of N,
 * it costs n*log(N) to update its output where N is the initial size of the interval
 *
 * @param lb is the lower bound of the interval
 * @param ub is the upper bound of the interval
 * @author renaud.delandtsheer@cetic.be
 * */
case class Interval(lb: CBLSIntVar, ub: CBLSIntVar) extends SetInvariant {
  assert(ub != lb)
  var output: CBLSSetVar = null

  def myMax = ub.maxVal
  def myMin = lb.minVal

  registerStaticAndDynamicDependency(lb)
  registerStaticAndDynamicDependency(ub)
  finishInitialization()

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output.setValue(SortedSet.empty[Int])
    if (lb.value <= ub.value)
      for (i <- lb.value to ub.value) output.insertValue(i)
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    if (v == lb) {
      if (OldVal < NewVal) {
        //intervale reduit
        if (OldVal <= ub.value)
          for (i <- OldVal to (ub.value min (NewVal-1))) output.deleteValue(i)
      }else{
        //intervale plus grand
        if (NewVal <= ub.value)
          for (i <- NewVal to (ub.value min (OldVal-1))) output.insertValue(i)
      }
    } else {
      if (OldVal > NewVal) {
        //intervale reduit
        if (lb.value <= OldVal)
          for (i <- (NewVal+1) max lb.value to OldVal) output.deleteValue(i)
      }else{
        //intervale plus grand
        if (lb.value <= NewVal)
          for (i <- (OldVal+1) max lb.value to NewVal) output.insertValue(i)
      }
    }
  }

  override def checkInternals(c: Checker) {
    c.check(output.value.size == 0.max(ub.value - lb.value + 1),
      Some("output.value.size (" + output.value.size
        + ") == 0.max(ub.value (" + ub.value
        + ") - lb.value (" + lb.value + ") + 1) ("
        + 0.max(ub.value - lb.value + 1) + ")"))
    if (ub.value >= lb.value) {
      for (i <- lb.value to ub.value)
        c.check(output.value.contains(i),
          Some("output.value.contains(" + i + ")"))
    }
  }
}

/**
 * maintains the output as any value taken from the intset var parameter.
 * if this set is empty, puts the default value ni output.
 * @param from where we take the value from
 * @param default the default value in case from is empty
 * @author renaud.delandtsheer@cetic.be
 * */
case class TakeAny(from: CBLSSetVar, default: Int) extends IntInvariant {
  def myMin: Int = from.getMinVal
  def myMax: Int = from.getMaxVal

  var output: CBLSIntVar = null
  registerStaticAndDynamicDependency(from)
  finishInitialization()

  var wasEmpty: Boolean = false

  def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)

    wasEmpty = from.value.isEmpty
    if (wasEmpty) {
      output := default
    } else {
      output := from.value.head
    }
  }

  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    if (wasEmpty) {
      output := value
      wasEmpty = false
    }
  }

  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    if (value == output.getValue(true)) {
      if (v.value.isEmpty) {
        output := default
        wasEmpty = true
      } else {
        output := from.value.head
      }
    }
  }

  override def checkInternals(c: Checker) {
    if (from.value.isEmpty) {
      c.check(output.value == default,
        Some("output.value (" + output.value
          + ") == default (" + default + ")"))
    } else {
      c.check(from.value.contains(output.value),
        Some("from.value.contains(output.value (" + output.value + "))"))
    }
  }
}
