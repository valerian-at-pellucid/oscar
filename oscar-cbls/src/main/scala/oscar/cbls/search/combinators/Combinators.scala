/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.search.combinators

import oscar.cbls.invariants.core.computation.{ CBLSSetVar, CBLSIntVar, Solution, Store }
import oscar.cbls.search.core.NoMoveFound
import oscar.cbls.search.core._
import oscar.cbls.search.move.{ CompositeMove, InstrumentedMove, Move }

import scala.language.implicitConversions

//TODO: les combinateurs devraient avoir une liste de voisinnages (ou neighborhood*), pas juste un seul.
//TODO: proposer du benchmarking des voisinages (nombre de moves trouvés, gain moyen sur une fct objectif, temps de recherche, nombre de recherche effectuées, ...)

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(a: Neighborhood*) extends Neighborhood {
  //this resets the internal state of the move combinators
  override def reset() {
    for (n <- a) n.reset()
  }

  override def verbose_=(i: Int): Unit = {
    for (n <- a) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + a.mkString(",") + ")"
}

class BasicProtectBest(a: Neighborhood, i: CBLSIntVar) extends NeighborhoodCombinator(a) {

  protected val s: Store = i.model

  protected var bestObj = if (currentSolutionIsAcceptable) i.value else Int.MaxValue
  protected var best: Solution = if (currentSolutionIsAcceptable) s.solution() else null


  //this resets the internal state of the move combinators
  override def reset(){
    super.reset()
    bestObj = Int.MaxValue
    best = null
  }

  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    //we record the obj before move to prevent an additional useless propagation
    val objBeforeMove = i.value

    a.getMove(acceptanceCriteria) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) =>
        if (m.objAfter > objBeforeMove && objBeforeMove < bestObj && currentSolutionIsAcceptable) {
          //solution degrades, and we were better than the best recorded
          //so we save
          best = s.solution(true)
          bestObj = objBeforeMove
          if (verbose >= 2) println("saving best solution before degradation (obj:" + bestObj + ")")
        }
        MoveFound(m)
    }
  }

  protected def currentSolutionIsAcceptable = true

  def restoreBest() {
    if (best == null) {
      if (verbose >= 1) println("no single acceptable solution seen")
    } else if (i.value > bestObj || !currentSolutionIsAcceptable) {
      s.restoreSolution(best)
      if (verbose >= 1) println("restoring best solution (obj:" + bestObj + ")")
    } else if (verbose >= 1) println("no better solution to restore")
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    val toReturn = doAllMoves(shouldStop, acceptanceCriterion)
    restoreBest()
    toReturn
  }

  def restoreBestOnExhaust(): RestoreBestOnExhaust = new RestoreBestOnExhaust(this)
}

class ProtectBest(a: Neighborhood, i: CBLSIntVar) extends BasicProtectBest(a: Neighborhood, i: CBLSIntVar) {

  def whenEmpty(violation: CBLSSetVar) = new ProtectBestWhen(a, i, () => violation.value.isEmpty)
  def whenZero(violation: CBLSIntVar) = new ProtectBestWhen(a, i, () => violation.value == 0)

  /**
   * this method restricts the save operation to only the situation where "shouldSave" returns true
   * notice that this is an override of the "when" method found in neighborhood.
   * @param shouldSave
   * @return
   */
  override def when(shouldSave: () => Boolean) = new ProtectBestWhen(a, i, shouldSave)
}

class ProtectBestWhen(a: Neighborhood, i: CBLSIntVar, shouldSave: () => Boolean) extends BasicProtectBest(a, i) {
  override protected def currentSolutionIsAcceptable: Boolean = shouldSave()
}

class RestoreBestOnExhaust(a: BasicProtectBest) extends NeighborhoodCombinator(a) {

  def restoreBest(): Unit = {
    a.restoreBest()
  }

  /**
   * same as doAllImprovingMoves and calling restoreBest after.
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept strictly improving moves
   * @return the number of moves performed
   */
  def doAllMovesAndRestoreBest(shouldStop: Int => Boolean, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {
    a.doAllMovesAndRestoreBest(shouldStop, acceptanceCriterion)
  }

  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriteria) match {
      case m: MoveFound => m
      case x =>
        restoreBest()
        x
    }
  }

}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move is asked to the neighborhood.
 * @param a a neighborhood
 * @param proc the procedure to execute before the neighborhood is queried
 */
class DoOnQuery(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    proc()
    a.getMove(acceptanceCriteria)
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move from this neighborhood is taken
 * The callBack is performed before the move is actually taken.
 * @param a a neighborhood
 * @param procBeforeMove the procedure to execute when the move is taken, , with the move as input parameter
 *                   use this to update a Tabu for instance
 * @param procAfterMove a procedure to execute after the move is taken, with the move as input parameter
 */
case class DoOnMove(a: Neighborhood,
                    procBeforeMove: Move => Unit = null,
                    procAfterMove: Move => Unit = null) extends NeighborhoodCombinator(a) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriteria) match {
      case m: MoveFound =>
        InstrumentedMove(m.m, callBackBeforeMove(m.m), callBackAfterMove(m.m))
      case x => x
    }
  }

  def callBackBeforeMove(m: Move)() {
    if (procBeforeMove != null) procBeforeMove(m)
  }

  def callBackAfterMove(m: Move)() {
    if (procAfterMove != null) procAfterMove(m)
  }
}

/**
 * this combinator attaches a custom code to a given neighborhood.
 * the code is called whenever a move from this neighborhood is taken for the first time.
 * notice that this neighborhood is reset, so first time can occur several times.
 * @param a a neighborhood
 * @param proc the procedure to call on one first move that is performed from this neighborhood
 */
class DoOnFirstMove(a: Neighborhood, proc: () => Unit) extends NeighborhoodCombinator(a) {
  var isFirstMove = true
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (isFirstMove) {
      a.getMove(acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, notifyMoveTaken)
        case x => x
      }
    } else {
      a.getMove(acceptanceCriteria)
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    isFirstMove = true
    super.reset()
  }

  private def notifyMoveTaken() {
    proc()
    isFirstMove = false
  }
}

/**
 * this combinator randomly tries one neighborhood.
 * it trie the other if the first did not find any move
 * @param a a neighborhood
 * @param b another neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class Random(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    var currentIsA: Boolean = math.random > 0.5
    def search(canDoMore: Boolean): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(acceptanceCriteria) match {
        case NoMoveFound =>
          currentIsA = !currentIsA; if (canDoMore) search(false) else NoMoveFound
        case x: MoveFound => x
      }
    }
    search(true)
  }
}

/**
 * this combinator sequentially tries all neighborhoods until one move is found
 * between calls, it will roll back to the first neighborhood
 * it tries a first, and if no move it found, tries b
 * a is reset if it did not find anything.
 * @param a a neighborhood
 * @param b another neighborhood
 * @author renaud.delandtsheer@cetic.be
 */
class OrElse(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        b.getMove(acceptanceCriteria)
      case x => x
    }
  }
}

/**
 * this combinator always selects the best move between the two parameters
 * notice that this combinator makes more sense
 * if the two neighborhood return their best found move,
 * and not their first found move, as usually done.
 * @author renaud.delandtsheer@cetic.be
 */
class Best(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {

  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    (a.getMove(acceptanceCriteria), b.getMove(acceptanceCriteria)) match {
      case (NoMoveFound, x) => x
      case (x, NoMoveFound) => x
      case (x: MoveFound, y: MoveFound) => if (x.objAfter < y.objAfter) x else y

    }
  }
}

/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it does not come back to the first one after the second one is exhausted
 * @author renaud.delandtsheer@cetic.be
 */
class Exhaust(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(acceptanceCriteria) match {
        case NoMoveFound => if (currentIsA) { currentIsA = false; search() } else NoMoveFound
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    super.reset()
  }
}

/**
 * retries n times the move before concluding to noMove can be found
 * resets o nhe first found move, or on reset
 * @param a the neighborhood on which we will perform retries
 * @param n the maximal number of retries on a before concluding it is dead
 */
class Retry(a: Neighborhood, n: Int = 1) extends NeighborhoodCombinator(a) {
  var remainingTries = n
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriteria) match {
      case NoMoveFound =>
        remainingTries -= 1
        if (remainingTries == 0) NoMoveFound
        else this.getMove(acceptanceCriteria)
      case x =>
        remainingTries = n
        x
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    remainingTries = n
  }
}

case class NoReset(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean) = a.getMove(acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset() {}
}
/**
 * this combinator is stateful.
 * it returns the result of one Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood.
 * it starts with Neighborhood a
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustBack(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            b.reset()
            b.getMove(acceptanceCriteria)
          } else {
            currentIsA = true
            a.reset()
            a.getMove(acceptanceCriteria)
          }
        case x: MoveFound => x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    super.reset()
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        a.getMove(acceptanceCriteria)
      case m: MoveFound => m
    }
  }
}

/**
 * this combinator is stateful.
 * it returns the result of the first Neighborhood until it returns NoMoveFound.
 * It then switches to the other Neighborhood,
 * but only if a move was found by the first neighborhood
 * it does not come back to the first one after the second one is exhausted
 * @author renaud.delandtsheer@cetic.be
 */
class ExhaustAndContinueIfMovesFound(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            movesFoundWithCurrent = false
            search()
          } else NoMoveFound
        case x: MoveFound =>
          movesFoundWithCurrent = true
          x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset() {
    currentIsA = true
    movesFoundWithCurrent = false
    super.reset()
  }
}

/**
 * this combinator is stateless, it checks the condition on every invocation. If the condition is false,
 * it does not try the Neighborhood and finds no move.
 * @author renaud.delandtsheer@cetic.be
 */
class Conditional(c: () => Boolean, b: Neighborhood) extends NeighborhoodCombinator(b) {
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (c()) b.getMove(acceptanceCriteria)
    else NoMoveFound
  }
}

/**
 * this combinator bounds the number of time the search is actually performed
 * @author renaud.delandtsheer@cetic.be
 */
class BoundSearches(a: Neighborhood, val maxMove: Int) extends NeighborhoodCombinator(a) {
  var remainingMoves = maxMove
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      remainingMoves -= 1
      a.getMove(acceptanceCriteria)
    } else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingMoves = maxMove
    super.reset()
  }
}

/**
 * this combinator bounds the number of moves done with this neighborhood
 * notice that the count is reset by the reset operation
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMoves(a: Neighborhood, val maxMove: Int) extends NeighborhoodCombinator(a) {
  var remainingMoves = maxMove
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (remainingMoves > 0) {
      a.getMove(acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, notifyMoveTaken)
        case x => x
      }
    } else {
      if (verbose >= 1)
        println("MaxMoves: reached " + (if(maxMove == 1) "1 move "else maxMove + " moves"))
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingMoves = maxMove
    super.reset()
  }

  def notifyMoveTaken() {
    remainingMoves -= 1
  }

  /**
   * to build a composite neighborhood.
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
   * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
   *
   */
  def andThen(b: Neighborhood) = new AndThen(a, b, maxMove)

  /**
   * this will modify the effect of the maxMoves by transforming it into a [[MaxMovesWithoutImprovement]]
   * the initial maxMoves is deleted by this method, and the integer bound is passed to [[MaxMovesWithoutImprovement]]
   */
  def withoutImprovementOver(obj: CBLSIntVar) = new MaxMovesWithoutImprovement(a, maxMove, obj)
}

/**
 * makes a round robin on the neighborhood. it swaps as soon as one does not find a move
 * and swaps neighborhood after "step" invocations
 * @author renaud.delandtsheer@cetic.be
 */
class RoundRobin(l: List[Neighborhood], steps: Int = 1) extends NeighborhoodCombinator(l: _*) {
  val robins = l.length
  var remainingSteps: Int = steps
  var tail: List[Neighborhood] = l
  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult =
    myGetImprovingMove(acceptanceCriteria)

  private def myGetImprovingMove(acceptanceCriteria: (Int, Int) => Boolean, triedRobins: Int = 0): SearchResult = {
    if (triedRobins >= robins) {
      NoMoveFound
    } else if (remainingSteps > 0) {
      //no need to change neighborhood yet
      remainingSteps -= 1
      tail.head.getMove(acceptanceCriteria) match {
        case NoMoveFound =>
          moveToNextRobin()
          myGetImprovingMove(acceptanceCriteria, triedRobins + 1)
        case x: MoveFound => x
      }
    } else {
      //move to next robin
      remainingSteps = steps
      moveToNextRobin()
      myGetImprovingMove(acceptanceCriteria, triedRobins + 1)
    }
  }

  private def moveToNextRobin() {
    if (tail.tail.isEmpty) {
      tail = l
    } else {
      tail = tail.tail
    }
    remainingSteps = steps
  }

  //this resets the internal state of the move combinators
  override def reset() {
    remainingSteps = steps
    super.reset()
  }

  /**
   * proposes a round-robin with that.
   * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative)
   * @param b
   * @return
   */
  override def step(b: Neighborhood): RoundRobin = new RoundRobin(l ::: List(b))
}

class RoundRobinNoParam(val a: Neighborhood, val b: Neighborhood) {
  def step(s: Int): Neighborhood = new RoundRobin(List(a, b), s)
}

object RoundRobinNoParam {
  implicit def toNeighBorHood(rr: RoundRobinNoParam): Neighborhood = {
    val toReturn = new RoundRobin(List(rr.a, rr.b), 1)
    toReturn.verbose = rr.a.verbose
    toReturn
  }
}

/**
 * to build a composite neighborhood.
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
 * @param a the first neighborhood, all moves delivered by this one will be considered
 * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
 * @param maxFirstStep the maximal number of moves to consider to the first neighborhood
 * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
 * @param stopAfterFirstIfEnough stops if an explored first move is enough; in this case the composite move is not explore, only the first move is returned.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class AndThen(a: Neighborhood, b: Neighborhood, maxFirstStep: Int = 10, maximalIntermediaryDegradation: Int = Int.MaxValue, stopAfterFirstIfEnough: Boolean = false) extends NeighborhoodCombinator(a, b) {

  /**
   * this method is called by AndThen to notify the first step, and that it is now exploring successors of this step.
   * this method is called before the step is actually taken.
   * @param m
   */
  def notifyFirstStep(m: Move) {}

  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    a.reset()

    var remainingFirstSteps = maxFirstStep

    var oldObj: Int = 0
    def instrumentedIntermediaryAcceptanceCriteria(stolenOldObj: Int, intermediaryObj: Int): Boolean = {
      oldObj = stolenOldObj
      intermediaryObj - stolenOldObj <= maximalIntermediaryDegradation
    }

    while (remainingFirstSteps > 0) {
      remainingFirstSteps -= 1
      a.getMove(instrumentedIntermediaryAcceptanceCriteria) match {
        case NoMoveFound => return NoMoveFound
        case MoveFound(firstMove) =>

          if (stopAfterFirstIfEnough) {
            if (acceptanceCriteria(oldObj, firstMove.objAfter)) {
              return firstMove
            }
          }

          val touchedVars = firstMove.touchedVariables
          val model = touchedVars.head.model
          val snapshot = model.saveValues(touchedVars: _*)
          notifyFirstStep(firstMove)
          firstMove.commit()
          if (amIVerbose) println("AndThen: trying first move " + firstMove)
          def globalAcceptanceCriteria: (Int, Int) => Boolean = (_, newObj) => acceptanceCriteria(oldObj, newObj)
          b.getMove(globalAcceptanceCriteria) match {
            case NoMoveFound => model.restoreSnapshot(snapshot)
            case MoveFound(secondMove) =>
              model.restoreSnapshot(snapshot)
              return CompositeMove(List(firstMove, secondMove),
                secondMove.objAfter,
                firstMove.neighborhoodName + "_AndThen_" + secondMove.neighborhoodName)
          }
      }
    }
    NoMoveFound
  }
}

/**
 * bounds the number of tolerated moves without improvements over the best value
 * the count is reset by the reset action.
 * @author renaud.delandtsheer@cetic.be
 */
class MaxMovesWithoutImprovement(a: Neighborhood, val maxMovesWithoutImprovement: Int, obj: CBLSIntVar) extends NeighborhoodCombinator(a) {

  var stepsSinceLastImprovement = 0
  var bestObj = Int.MaxValue

  override def getMove(acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {
    if (stepsSinceLastImprovement <= maxMovesWithoutImprovement) {
      a.getMove(acceptanceCriteria) match {
        case m: MoveFound => InstrumentedMove(m.m, callBack = null, afterMove = notifyMoveTaken)
        case x => x
      }
    } else {
      if (verbose >= 1) println("MaxStepsWithoutImprovement: reached " + maxMovesWithoutImprovement + " moves without improvement")
      NoMoveFound
    }
  }

  //this resets the internal state of the move combinators
  override def reset() {
    stepsSinceLastImprovement = 0
    super.reset()
  }

  def notifyMoveTaken() {
    val newObj = obj.value
    if (newObj < bestObj) {
      bestObj = newObj
      stepsSinceLastImprovement = 0
    } else {
      stepsSinceLastImprovement += 1
    }
  }
}

/**
 * calls the neighborhood until an improvement over obj is achieved
 * the improvement is "since the last reset"
 * @param a
 * @param minMoves the min number of queries that will be forwarded to a (priority over the improvement)
 * @param maxMove the max number of queries that will be forwarded to a (priority over the improvement)
 * @param obj the obj that is looked for improvement
 * @author renaud.delandtsheer@cetic.be
 */
class UntilImprovement(a: Neighborhood, obj: CBLSIntVar, val minMoves: Int = 0, val maxMove: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a) {

  //TODO: pas sûr que cela fonctionne du premier coup; peut-être faut-il faire un reset au début de toute descente.
  var oldObjOnReset = obj.value
  var movesQueriedSinceReset = 0

  override def getMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    movesQueriedSinceReset += 1
    if (movesQueriedSinceReset < maxMove
      && (movesQueriedSinceReset < minMoves || obj.value >= oldObjOnReset))
      a.getMove(acceptanceCriterion)
    else NoMoveFound
  }

  //this resets the internal state of the move combinators
  override def reset() {
    oldObjOnReset = obj.value
    movesQueriedSinceReset = 0
    super.reset()
  }
}

/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 * @param a
 * @param name
 */
class Name(a: Neighborhood, val name: String) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    a.getMove(acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => CompositeMove(List(m), m.objAfter, name)
    }
  }
}

/**
 * tis combinator overrides the acceptance criterion given to the whole neighborhood
 * this can be necessary if you have a neighborhood with some phases only including simulated annealing
 * @param a the neighborhood
 * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall sear
 */
class WithAcceptanceCriterion(a: Neighborhood, overridingAcceptanceCriterion: (Int, Int) => Boolean) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion this criterion is not considered by this combinator.
   * @return an improving move
   */
  override def getMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = a.getMove(overridingAcceptanceCriterion)
}

/**
 * this combinator injects a metropolis acceptation function.
 * the criterion accepts all improving moves, and for worsening moves, it applies the metropolis criterion:
 * accept if math.random(0.0; 1.0) < math.exp(-gain / temperatureValue)
 * @param a
 * @param temperature a function that inputs the number of moves taken, and outputs a temperature, for use in the criterion
 *                    the number of steps is reset to zero when the combinator is reset
 */
class Metropolis(a: Neighborhood, temperature: Int => Float = _ => 100) extends NeighborhoodCombinator(a) {

  var moveCount = 0
  var temperatureValue: Float = temperature(moveCount)
  override def getMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult =
    a.getMove(acceptation) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => InstrumentedMove(m, notifyMoveTaken)
    }

  def acceptation(oldObj: Int, newObj: Int): Boolean = {
    val gain = oldObj - newObj
    if (gain > 0) return true
    // metropolis criterion
    return math.random < math.exp(-gain / temperatureValue)
  }

  def notifyMoveTaken() {
    moveCount += 1
    temperatureValue = temperature(moveCount)
  }

  //this resets the internal state of the move combinators
  override def reset() {
    super.reset()
    moveCount = 0
    temperatureValue = temperature(moveCount)
  }
}
