/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 23/10/12
 * Time: 16:17
 * To change this template use File | Settings | File Templates.
 */
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
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/
package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.model._

/*

 */
object ThreeOptC extends SearchEngine{


  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    , k:Int):Neighbor = findMove(false, vrp, k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    , k:Int, prevmove:Neighbor = null):Neighbor= findMove(true,vrp, k, prevmove)

  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, previousMove:Neighbor = null):ThreeOptC = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null


    val hotRestart = if (previousMove == null) 0 else previousMove.startNodeForNextExploration
    var endOfFirstEdge:Int = 0

    for (startOfFirstEdge <- 0 until vrp.N startBy hotRestart if (vrp.isRouted(startOfFirstEdge))){
      endOfFirstEdge = vrp.Next(startOfFirstEdge).value

      for (startOfThirdEdge <- vrp.getKNearestNeighbors(k,endOfFirstEdge)
           if ( vrp.isRouted(startOfThirdEdge)&&
             vrp.isASegment(startOfFirstEdge,vrp.Next(startOfThirdEdge).value) &&
             vrp.isAtLeastAsFarAs(endOfFirstEdge,startOfThirdEdge,3))){// filter

        for (startOfSecondEdge <- vrp.getKNearestNeighbors(k,vrp.Next(startOfFirstEdge).value)
             if ((startOfSecondEdge != endOfFirstEdge && vrp.Next(startOfSecondEdge).value != startOfThirdEdge) &&
               vrp.isRouted(startOfSecondEdge) &&
               (vrp.isASegment(endOfFirstEdge,vrp.Next(startOfSecondEdge).value))&&
               vrp.isBetween(vrp.Next(startOfSecondEdge).value,startOfFirstEdge,startOfThirdEdge)))
        {
          val newObj = getObjAfterMove(startOfFirstEdge,startOfSecondEdge ,startOfThirdEdge,vrp)
          if (newObj < BestObj){
            if(FirstImprove)
              return ThreeOptC(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,newObj, vrp)
            BestObj = newObj
            move = ((startOfFirstEdge, startOfSecondEdge ,startOfThirdEdge ))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptC(move._1, move._2, move._3, BestObj, vrp)
  }

  /*Performs the three-opt move with two reverse segment. */

  def doMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,vrp:VRP)  {
    val toUpdate = vrp.threeOptC(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }



  def getObjAfterMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.threeOptC(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    vrp.getAssignVal(toUpdate)
  }
}

case class ThreeOptC(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptC.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  override def toString():String =  "(firstEdge = " + startOfFirstEdge + ", secondEdge = " + startOfSecondEdge + ", " +
    "thirdEdge = "+ startOfThirdEdge+" )"
  def startNodeForNextExploration: Int = startOfFirstEdge
}

