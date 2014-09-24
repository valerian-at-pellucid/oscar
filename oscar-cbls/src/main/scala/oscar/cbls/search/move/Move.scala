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

package oscar.cbls.search.move

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar, Variable}
import oscar.cbls.objective.Objective

/** standard move template
  *
  * @param objAfter the objective after this assignation will be performed
  *                 in case you degrade the objective because you make a jump, and you do not want to compute it,
  *                 you must set it to Int.MaxValue or just do not specify it, as it is the default value
  *                 we did not use an option there because there would anyway be a need
  *                 for arithmetic on this option in combinators suh as [[oscar.cbls.search.combinators.Best]]
  *                 Many combinators actually rely on this value to take decisions (eg: [[oscar.cbls.search.combinators.ProtectBest]] and [[oscar.cbls.search.combinators.Best]]
  * @author renaud.delandtsheer@cetic.be
  */
abstract class Move(val objAfter:Int = Int.MaxValue, val neighborhoodName:String = null){
  /**to actually take the move*/
  def commit()

  /**
   * to get the list of variables that are modified by the move.
   * use this to update a Tabu for instance
   * notice that is a variable is touched twice by the move, it will appear twice in this list
   * This can happen with a set where we add two elements in two distinct moves that are aggregated into a [[oscar.cbls.search.move.CompositeMove]]
   * @return the list of touched variables.
   */
  def touchedVariables:List[Variable] = throw new Exception(this.getClass().getSimpleName + "cannot provide touched variables")

  /**
   * @return a readable string of the objective after wit ha space before, or an empty string
   */
  def objToString:String = if(objAfter == Int.MaxValue) "" else " objAfter:" +objAfter

  def neighborhoodNameToString:String = if (neighborhoodName != null) neighborhoodName + ": " else ""

  /** this performs the move, evaluates he objective function, and backtracks the move
    *notice that the objAfter is supposed to carry the proper value, so you generally do not need to call this
    * since it is not an efficient way to proceed
    * notice that it relies on the touchedVariables method.
    * @param obj the objective function to evaluate
    * @return the value of the objective function if the move were taken
    */
  def evaluate(obj:Objective):Int = {
    val model = obj.objective.model
    val snapshot = model.saveValues(touchedVariables:_*)
    commit()
    val toReturn = obj.value
    model.restoreSnapshot(snapshot)
    toReturn
  }
}

object Move{
  def apply(objAfter:Int = 0, neighborhoodName:String = null)(code: =>Unit):EasyMove = new EasyMove(objAfter, neighborhoodName, code)
}
/**
 * this class does not provide an implementation for touchedVariables,
 * since we are only inputting source code for executing the move
 * */
class EasyMove(override val objAfter:Int, override val neighborhoodName:String = null, code: => Unit)
  extends Move(objAfter, neighborhoodName){

  override def commit() {code}

  override def toString: String = neighborhoodNameToString + "EasyMove"

}

/** standard move that assigns an int value to a CBLSIntVar
  *
  * @param i the variable
  * @param v the value to assign
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AssignMove(i:CBLSIntVar,v:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {i := v}

  override def toString: String = {
    neighborhoodNameToString + "AssignMove(" + i + " set to " + v + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(i)
}

/** standard move that swaps the value of two CBLSIntVar
  *
  * @param i the variable
  * @param j the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {i :=: j}

  override def toString: String  = {
    neighborhoodNameToString + "SwapMove(" + i + " swapped with " + j + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(i,j)
}

/** standard move that adds a value to a CBLSSetVar
  *
  * @param s the variable
  * @param v the value to add to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AddToSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {s :+= v}

  override def toString: String = {
    neighborhoodNameToString + "AddToSetMove(" + s + " :+= " + v + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(s)
}

/** standard move that removes a value to a CBLSSEtVar
  *
  * @param s the variable
  * @param v the value to remove to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class RemoveFromSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit() {s :-= v}

  override def toString: String = {
    neighborhoodNameToString + "RemoveFromSetMove(" + s + " :-= " + v + objToString + ")"
  }

  override def touchedVariables: List[Variable] = List(s)
}

/** a composition of a list of moves; the move will be taken in the order given by the list
  *
  * @param ml the list of moves constituting this one
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class CompositeMove(ml:List[Move], override val objAfter:Int, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  def this(ml:List[Move]){
    this(ml, ml.last.objAfter)
  }

  override def commit() {
    for(m <- ml) m.commit()
  }

  override def toString: String  = {
    neighborhoodNameToString + "CompositeMove(" + ml.mkString(" and ") + objToString + ")"
  }

  override def touchedVariables: List[Variable] = ml.flatMap(_.touchedVariables)
}

/** an instrumented move that performs a callBack before being taken
  * the move itself is given in parameter.
  *
  * @param initialMove the actual move
  * @param callBack the method to invoke before the actual move is taken
  * @author renaud.delandtsheer@cetic.be
  */
case class InstrumentedMove(initialMove:Move, callBack: () => Unit, afterMove: () => Unit = null) extends Move(initialMove.objAfter, initialMove.neighborhoodName){
  def commit(){
    if(callBack != null) callBack()
    initialMove.commit()
    if(afterMove != null) afterMove()
  }

  override def toString: String = initialMove.toString

  override def touchedVariables: List[Variable] = initialMove.touchedVariables
}

object CallBackMove{
  def apply(callBack: () => Unit, objAfter:Int, neighborhoodName:String, shortDescription:() => String)=
    new CallBackMove[Unit](_ => {callBack()}, objAfter, neighborhoodName, shortDescription)
}

case class CallBackMove[T](callBack: T => Unit, override val objAfter:Int, override val neighborhoodName:String, shortDescription:() => String, param:T = null) extends Move{
  def commit(){
    callBack(param)
  }

  override def toString: String = {
    neighborhoodNameToString + (if (shortDescription != null) shortDescription() else "")
  }
}
