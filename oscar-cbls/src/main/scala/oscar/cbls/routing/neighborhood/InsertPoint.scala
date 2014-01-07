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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 *     Refactored with respect to the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._
import scala.util.Random
import oscar.cbls.search.SearchEngineTrait

/**
 * Inserts an unrouted point in a route.
 * The search complexity is O(n²).
 */
object InsertPoint extends Neighborhood with SearchEngineTrait {
  override protected def doSearch(
      s: SearchZone,
      moveAcceptor: (Int) => (Int) => Boolean,
      returnMove: Boolean): SearchResult = {
    val startObj: Int = s.vrp.getObjective()
    s.vrp.cleanRecordedMoves()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      //TODO: bizarre qu'on itère à l'envers sur les points d'insertion puis sur les points à insérer!
      //il faudrait itérer sur les points non routés, puis sur les poijnts d'insertion parmi les points relevants.
      //tant pis pour le primaryIterator en fait.
      val beforeInsertedPoint: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(beforeInsertedPoint)) {
        for (
          insertedPoint <- s.relevantNeighbors(beforeInsertedPoint) if (
            !vrp.isRouted(insertedPoint))) {

          assert(s.vrp.isRecording, "MoveDescription should be recording now")

          encode(beforeInsertedPoint, insertedPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) {
                return MoveFound(InsertPoint(beforeInsertedPoint,
                  insertedPoint, newObj, vrp))
              } else return MovePerformed()
            }
            case _ => ()
          }
        }
      }
    }
    NoMoveFound()
  }

  def encode(beforeInsertedPoint: Int, insertedPoint: Int, vrp: VRP with MoveDescription) {
    assert(!vrp.isRouted(insertedPoint))
    val s = vrp.segmentFromUnrouted(insertedPoint)
    vrp.insert(s, beforeInsertedPoint)
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeInsertedPoint the place where to insert an unrouted point.
 * @param insertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param vrp the given VRP problem.
 */
case class InsertPoint(
  beforeInsertedPoint: Int,
  insertedPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    InsertPoint.encode(beforeInsertedPoint, insertedPoint, vrp)
  }

  override def toString: String = "InsertPoint(beforeInsertedPoint = " + beforeInsertedPoint + ", insertedPoint = " + insertedPoint + " )"
}