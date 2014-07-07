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

package oscar.cbls.search.moves

import oscar.cbls.invariants.core.computation.{Store, CBLSIntVar}
import scala.language.implicitConversions

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult
case object ProblemSolved extends SearchResult
//case object MovePerformed extends SearchResult

case class MoveFound(m:Move) extends SearchResult{
  def commit(){m.commit()}
  def objAfter = m.objAfter
}

object SearchResult {
  implicit def moveToSearchResult(m: Move): MoveFound = MoveFound(m)
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood{
//  def getImprovingMove(acceptor:(Int,Int) => Boolean):SearchResult =  getImprovingMove()

  def getImprovingMove():SearchResult

  //this resets the internal state of the Neighborhood
  def reset()

  /** verbosity: 0: none
    * 1: combinators
    * 2: combinators + neighborhoods
    */
  var _verbose:Int = 0
  def verbose:Int = _verbose
  def verbose_=(i:Int){
    _verbose = i
  }

  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove():Boolean = (0 != doAllImprovingMoves(1))

  /**
   * @return the number of moves performed
   */
  def doAllImprovingMoves(maxMoves:Int = Int.MaxValue):Int = {
    var toReturn = 0;
    var remainingMoves = maxMoves
    while(remainingMoves != 0){
      getImprovingMove() match {
        case ProblemSolved => {
          if (verbose >= 1) println("problem solved after " + toReturn + " it")
          return toReturn;
        }
        case NoMoveFound => {
          if (verbose >= 1) println("no move found after " + toReturn + " it")
          return toReturn;
        }
        case m: MoveFound => {
          if (verbose >= 1) println(m)
          m.commit
          true
        }
      }
      toReturn += 1
      remainingMoves -= 1
    }
    if(verbose >= 1)println("maxMoves ("+ maxMoves+") performed")
    toReturn
  }

  def random(b:Neighborhood):Neighborhood = new Random(this,b)
  def orElse(b:Neighborhood):Neighborhood = new OrElse(this,b)
  def best(b:Neighborhood):Neighborhood = new Best(this,b)
  def exhaust(b:Neighborhood):Neighborhood = new Exhaust(this,b)
  def exhaustBack(b:Neighborhood):Neighborhood = new ExhaustBack(this,b)
  def exhaustAndContinueIfMovesFound(b:Neighborhood) = new ExhaustAndContinueIfMovesFound(this, b)
  def when(c:()=>Boolean):Neighborhood = new Conditional(c, this)
  def maxSearches(maxMove:Int) = new BoundSearches(this, maxMove)
  def maxMoves(maxMove:Int) = new BoundMoves(this, maxMove)
  def roundRobin(b:Neighborhood):RoundRobinNoParam = new RoundRobinNoParam(this,b)
  def onQuery(proc:  => Unit) = new DoOnQuery(this,() => proc)
  def onMove(proc: => Unit) = new DoOnMove(this,() => proc)
  def onFirstMove(proc: => Unit) = new  DoOnFirstMove(this,() => proc)
  def protectBest(i:CBLSIntVar) = new ProtectBest(this, i)
  def retry() = new Retry(this)

}

abstract class StatelessNeighborhood extends Neighborhood{
  //this resets the internal state of the move combinators
  final override def reset(){}

  override def toString: String = this.getClass.getSimpleName
}

/** a neighborhood that never finds any move (quite useless, actually)
  */
case class NoMoveNeighborhood() extends StatelessNeighborhood{
  override def getImprovingMove(): SearchResult = NoMoveFound
}

case class AssignMove(i:CBLSIntVar,v:Int, override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

  override def commit() {i := v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "AssignMove(" + i + " set to " + v + " objAfter:" + objAfter + ")"
  }
}
