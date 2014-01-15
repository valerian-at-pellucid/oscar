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
 *            Yoann Guyot
 ******************************************************************************/


package oscar.cbls.invariants.lib.logic
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * maintains a sorting of the ''values'' array:
 * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
 * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
 * */
case class Sort(var values:Array[CBLSIntVar], ReversePerm:Array[CBLSIntVar]) extends Invariant {
  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  finishInitialization()

  //position in initial array -> position in sort
  val ForwardPerm:Array[CBLSIntVar]=ReversePerm.map(i => CBLSIntVar(this.model,0,values.size,0,"ForwardPerm"))

  //reverse perm: position in sort -> position in initial array

  assert(values.indices == ForwardPerm.indices)
  assert(values.indices == ReversePerm.indices)
  ReversePerm.foreach(i => i.setDefiningInvariant(this))
  ForwardPerm.foreach(i => i.setDefiningInvariant(this))

  {
    //initial sort of the variables, this is in brackets to free Sorting asap
    val Sorting: Array[Int] = values.indices.toArray.sortBy(indice => values(indice).value)
    //sorting is position in sorting -> position in initial array
    for (i <- values.indices) {
      ReversePerm(i) := Sorting(i)
      ForwardPerm(Sorting(i)) := i
    }
  }

  //returns the reverse permutation of the sort.
  def GetForwardPerm(): Array[CBLSIntVar] = ForwardPerm

  @inline
  override def notifyIntChanged(v: CBLSIntVar, index: Int, OldVal: Int, NewVal: Int) {
    if (NewVal > OldVal) BubbleUp(v, index)
    else BubbleDown(v, index)
  }

  @inline
  private def BubbleUp(v: CBLSIntVar, PositionInInitialArray: Int) {
    while (true) {
      val PositionInSorting: Int = ForwardPerm(PositionInInitialArray).getValue(true)
      if (PositionInSorting == values.indices.last) return //last position
      val ValueAbove: Int = values(ReversePerm(PositionInSorting + 1).getValue(true)).value //this shit returns the new value!!
      if (ValueAbove < v.value) swap(PositionInSorting, PositionInSorting + 1)
      else return
    }
  }

  @inline
  private def BubbleDown(v: CBLSIntVar, PositionInInitialArray: Int) {
    while (true) {
      val PositionInSorting: Int = ForwardPerm(PositionInInitialArray).getValue(true)
      if (PositionInSorting == 0) return //first position
      val ValueBelow: Int = values(ReversePerm(PositionInSorting - 1).getValue(true)).value
      if (ValueBelow > v.value) swap(PositionInSorting, PositionInSorting - 1)
      else  return
    }
  }

  @inline
  private def swap(PositionInSorting1: Int, PositionInSorting2: Int) {
    val PositionInInitialArray1: Int = ReversePerm(PositionInSorting1).getValue(true)
    val PositionInInitialArray2: Int = ReversePerm(PositionInSorting2).getValue(true)

    ReversePerm(PositionInSorting1) := PositionInInitialArray2
    ReversePerm(PositionInSorting2) := PositionInInitialArray1

    ForwardPerm(PositionInInitialArray1) := PositionInSorting2
    ForwardPerm(PositionInInitialArray2) := PositionInSorting1
  }

  override def checkInternals(c: Checker) {
    val range = values.indices
    for (i <- range) {
      c.check(ReversePerm(ForwardPerm(i).value).value == i,
        Some("ReversePerm(ForwardPerm(" + i
          + ").value ("+ForwardPerm(i).value+")).value  ("+ ReversePerm(ForwardPerm(i).value).value+") == " + i))
      c.check(ForwardPerm(ReversePerm(i).value).value == i,
        Some("ForwardPerm(ReversePerm(" + i
          + ").value ("+ReversePerm(i).value+")).value ("+ ForwardPerm(ReversePerm(i).value).value+ ") == " + i))
    }
    for (i <- range) {
      for (j <- range if i < j) {
        c.check((values(ReversePerm(i).getValue(true)).value <= values(ReversePerm(j).getValue(true)).value),
          Some("(values(ReversePerm(" + i + ").getValue(true)).value ("
            + values(ReversePerm(i).getValue(true)).value
            + ") <= values(ReversePerm(" + j + ").getValue(true)).value "
            + values(ReversePerm(j).getValue(true)).value + ")"))
      }
    }
  }
}

/**Helper object for the sort invariant, creates the necessary variables*/
object Sort {
  /**
   * returns the ForwardPerm for a given array
   * It instantiates an array of the appropriate size and populates it with IntVar.
   */
  def MakeSort(values: Array[CBLSIntVar]): Sort = {
    val m: Store = InvariantHelper.findModel(values)
    val ReversePerm: Array[CBLSIntVar] = values.map(v => CBLSIntVar(m, values.indices.start, values.indices.end, 0, "reverse_perm"))
    Sort(values, ReversePerm)
  }
}
