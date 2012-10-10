package oscar.cbls.routing

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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.{Snapshot, IntVar}
import collection.immutable.SortedMap;

/**moves a point in a circuit to another place.
 * size if O(n²)
 */
object OnePointMove extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction):OnePointMove = findMove(false, vrp)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null):OnePointMove = findMove(true,vrp, startFrom)
  //def justMove(vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null) {getFirstImprovingMove(vrp, startFrom).comit}

  /**Search for the proper One point move
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one
   * @param vrp the model of the problem
   */
  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null):OnePointMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val StartBeforeMovedPoint = if (startFrom == null) 0 else startFrom.startNodeForNextExploration
    def nextModulo(n:Int):Int = {
      if (n+1 >= (vrp.N)) 0
      else n+1
    }
    var beforeMovedPoint = StartBeforeMovedPoint

    do{
      if (vrp.Next(beforeMovedPoint).value >= vrp.V){
        var putAfter = 0
        do{
          if (beforeMovedPoint != putAfter && vrp.Next(beforeMovedPoint).value != putAfter && vrp.Next(putAfter).value != 0){
            val newObj = getObjAfterMove(beforeMovedPoint,putAfter, vrp)
            if (newObj < BestObj){
              if (FirstImprove){
                return OnePointMove(beforeMovedPoint,putAfter, newObj, vrp)
              }
              BestObj = newObj
              move = ((beforeMovedPoint, putAfter))
            }
          }
          putAfter = nextModulo(putAfter)
         }while(putAfter != 0)
      }
      beforeMovedPoint = nextModulo(beforeMovedPoint)
      }while(beforeMovedPoint != StartBeforeMovedPoint)
    if (move == null) null
    else OnePointMove(move._1,move._2, BestObj, vrp)
  }

  def doMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP){
     val toUpdate =vrp.moveSegmentVariablesToUpdate(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
     toUpdate.foreach(t => t._1 := t._2)
   }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.moveSegmentVariablesToUpdate(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
    vrp.getAssignVal(toUpdate)
  }
}

case class OnePointMove(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {OnePointMove.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "moved point " + vrp.Next(predOfMovedPoint).value + " after " + PutAfter

  def startNodeForNextExploration: Int = predOfMovedPoint
}

