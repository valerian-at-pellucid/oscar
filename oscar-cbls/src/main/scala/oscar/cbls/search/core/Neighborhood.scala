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

package oscar.cbls.search.core

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.objective.Objective
import oscar.cbls.search.combinators._
import oscar.cbls.search.move.Move

import scala.language.implicitConversions

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult

case class MoveFound(m:Move) extends SearchResult{
  def commit(){m.commit()}
  def objAfter = m.objAfter
  override def toString():String = m.toString()
}

object SearchResult {
  implicit def moveToSearchResult(m: Move): MoveFound = MoveFound(m)
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood{
//  def getImprovingMove(acceptor:(Int,Int) => Boolean):SearchResult =  getImprovingMove()

  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                         beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  def getImprovingMove(acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):SearchResult

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

  protected def amIVerbose = verbose >= 2
  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove():Boolean = (0 != doAllImprovingMoves(_ >= 1))

  /**
   * @return the number of moves performed
   */

  /**
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   * @return
   */
  def doAllImprovingMoves(shouldStop:Int => Boolean, acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):Int = {
    var toReturn = 0
    var moveCount = 0
    while(!shouldStop(moveCount)){
      getImprovingMove(acceptanceCriterion) match {
        case NoMoveFound =>
          if (verbose >= 1) println("no more move found after " + toReturn + " it")
          return toReturn;
        case m: MoveFound =>
          if (verbose >= 1) println(m)
          m.commit()
          true
      }
      toReturn += 1
      moveCount += 1
    }
    if(verbose >= 1)println("stop criteria met after "+ moveCount+" moves")
    toReturn
  }

  /** this composer randomly tries one neighborhood.
    * it tries the other if the first did not find any move
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def random(b:Neighborhood):Neighborhood = new Random(this,b)

  /** this composer sequentially tries all neighborhoods until one move is found
    * between calls, it will roll back to the first neighborhood
    * it tries a first, and if no move it found, tries b
    * a is reset if it did not find anything.
    * @param b another neighborhood
    * @author renaud.delandtsheer@cetic.be
    */
  def orElse(b:Neighborhood):Neighborhood = new OrElse(this,b)

  /**this composer always selects the best move between the two parameters
    * notice that this combinator makes more sense
    * if the two neighborhood return their best found move,
    * and not their first found move, as usually done.
    * @author renaud.delandtsheer@cetic.be
    */
  def best(b:Neighborhood):Neighborhood = new Best(this,b)

  /**this composer is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it does not come back to the first one after the second one is exhausted
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaust(b:Neighborhood):Neighborhood = new Exhaust(this,b)

  /**this composer is stateful.
    * it returns the result of one Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood.
    * it starts with Neighborhood a
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustBack(b:Neighborhood):Neighborhood = new ExhaustBack(this,b)

  /**this composer is stateful.
    * it returns the result of the first Neighborhood until it returns NoMoveFound.
    * It then switches to the other Neighborhood,
    * but only if a move was found by the first neighborhood
    * it does not come back to the first one after the second one is exhausted
    * @author renaud.delandtsheer@cetic.be
    */
  def exhaustAndContinueIfMovesFound(b:Neighborhood) = new ExhaustAndContinueIfMovesFound(this, b)

  /**this composer is stateless, it checks the condition on every invocation. If the condition is false,
    * it does not try the Neighborhood and finds no move.
    * @author renaud.delandtsheer@cetic.be
    */
  def when(c:()=>Boolean):Neighborhood = new Conditional(c, this)

  /**this one bounds the number of time the search is actually performed
    * notice that the count is reset by the reset operation
    * @author renaud.delandtsheer@cetic.be
    */
  def maxSearches(maxMove:Int) = new BoundSearches(this, maxMove)

  /**this one bounds the number of moves done with this neighborhood
    * notice that the count is reset by the reset operation
    * @author renaud.delandtsheer@cetic.be
    */
  def maxMoves(maxMove:Int) = new MaxMoves(this, maxMove)

  /**makes a round robin on the neighborhood. it swaps as soon as one does not find a move
    * and swaps neighborhood after "step" invocations
    * @author renaud.delandtsheer@cetic.be
    */
  def roundRobin(b:Neighborhood):RoundRobinNoParam = new RoundRobinNoParam(this,b)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move is asked to the neighborhood.
    * @param proc the procedure to execute before the neighborhood is queried
    */
  def onQuery(proc:  => Unit) = new DoOnQuery(this,() => proc)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken
    * The callBack is performed before the move is actually taken.
    * @param proc the procedure to execute when the move is taken
    */
  def onMove(proc: => Unit) = new DoOnMove(this,() => proc)

    /** this combinator attaches a custom code to a given neighborhood.
      * the code is called whenever a move from this neighborhood is taken
      * is gets the applied move in input.
      * The callBack is performed before the move is actually taken.
      * @param procOnMove a procedure that inputs the move that is applied;
      *                   use this to update a Tabu for instance
      */
  def onMove(procOnMove:Move => Unit) = new DoOnMove(this,null,procOnMove)

  /** this combinator attaches a custom code to a given neighborhood.
    * the code is called whenever a move from this neighborhood is taken for the first time.
    * notice that this neighborhood is reset, so first time can occur several times.
    * @param proc the procedure to call on one first move that is performed from this neighborhood
    */
  def onFirstMove(proc: => Unit) = new  DoOnFirstMove(this,() => proc)
  def protectBest(i:CBLSIntVar) = new ProtectBest(this, i)
  def protectBest(o:Objective) = new ProtectBest(this, o.objective)

  /** retries n times the move before concluding to noMove can be found
    * resets o nhe first found move, or on reset
    * @param n the maximal number of retries on a before concluding it is dead
    */
  def retry(n:Int = 1) = new Retry(this,n)

  /**to build a composite neighborhood.
    * the first neighborhood is used only to provide a round robin exploration on its possible moves
    * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
    * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
    *
    * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
    * and we consider the composition of the two moves for the acceptance criteria.
    * the returned move is the composition of the two found moves
    *
    * you must also ensure that the two neighborhood evaluate the same objective function,
    * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
    *
    * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
    * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
    *
    * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
    *
    * notice that you can use the following better syntax:
    * {{{
    *   myFirstNeighborhood maxMoves 5 andThen mySecondNeighborhood
    * }}}
    *
    * since the proper mechanism is built into Neighborhood.maxMoves; see [[oscar.cbls.search.combinators.MaxMoves]]
    *
    * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
    * @param maxFirstStep the maximal number of moves to consider to the first neighborhood
    *
    * @author renaud.delandtsheer@cetic.be
    */
  def andThen(b:Neighborhood, maxFirstStep:Int) = new AndThen(this, b, maxFirstStep)

}

abstract class StatelessNeighborhood extends Neighborhood{
  //this resets the internal state of the move combinators
  final override def reset(){}

  override def toString: String = this.getClass.getSimpleName
}

/** a neighborhood that never finds any move (quite useless, actually)
  */
case class NoMoveNeighborhood() extends StatelessNeighborhood{
  override def getImprovingMove(acceptanceCriterion:(Int,Int) => Boolean): SearchResult = NoMoveFound
}
