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
package oscar.dfo.multiobjective.mogen

import oscar.dfo.utils.FeasibleRegion
import oscar.dfo.utils.MOEvaluator
import oscar.util.RandomGenerator
import oscar.dfo.utils.MOOPoint
import oscar.algo.paretofront.LinearList
import oscar.algo.paretofront.ParetoFront
import scala.Array.canBuildFrom
import oscar.dfo.multiobjective.mogen.algos.ComparativeAlgorithm

class MOGEN(var evaluator: MOEvaluator) {

  /** Function defining the feasible region of the problem */
  val feasibleRegion = FeasibleRegion()
  /** The set of non-dominated points (approximation of the Pareto front) */
  var archive = LinearList[Double, MOGENTriplet]()
  /** The iterate selection heuristic */
  var selectionHeuristic: ParetoFront[Double, MOGENTriplet] => MOGENTriplet = MOGEN.fairSelect

  def initFeasibleReagion(feasibilityFunctions: List[Array[Double] => Boolean]) = for (newFun <- feasibilityFunctions) feasibleRegion.addFunction(newFun)

  def initArchive(maxNbPoints: Int, startIntervals: Array[(Double, Double)], algorithms: List[(ComparativeAlgorithm, Double)]): Unit = {
    for (i <- 1 to 100) {
      val newCoordinates = startIntervals.map(elem => elem._1 + RandomGenerator.nextDouble * (elem._2 - elem._1))
      if (feasibleRegion.isFeasible(newCoordinates.toArray)) {
        val newAlgo = getRandomAlgo(algorithms)
        val newAlgoState = newAlgo.getInitialState(newCoordinates, startIntervals, evaluator, feasibleRegion)
        for (newPoint <- newAlgoState.getPoints) {
          val newTriplet = MOGENTriplet(newPoint, newAlgo, newAlgoState.getNewState(newPoint))
          archive.insert(newTriplet)
          if (archive.size >= maxNbPoints) return
        }
      }
    }
  }

  def getRandomAlgo(algorithms: List[(ComparativeAlgorithm, Double)]): ComparativeAlgorithm = {
    val sumOfProbas = algorithms.foldLeft(0.0)((acc, newPair) => acc + newPair._2)
    val randNum = RandomGenerator.nextDouble * sumOfProbas
    var accSum = 0.0
    for ((algo, proba) <- algorithms) {
      accSum += proba
      if (accSum > randNum) return algo
    }
    algorithms(0)._1
  }

  def optimizeMOO(maxIters: Int): Set[MOOPoint] = {
    var nbIterations = 1
    while (nbIterations <= maxIters) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    println("NB EVALS: " + evaluator.nbCallToEvalFunction)
    archive.toSet.map((e: MOGENTriplet) => e.getMOOPoint)
  }
  
  def performIteration(iterationNumber: Int) {
    val currentTriplet = selectIterate
    MOGEN.onIterateSelected(currentTriplet)
    println("What?")
    //if (visu) {paretoPlot.highLightIterate(currentTriplet); Thread.sleep(500)}
    val newPoints = currentTriplet.getAlgorithm.singleIteration(currentTriplet.getAlgorithmState, archive, feasibleRegion, evaluator)
    var archiveChanged = false
    for (newPoint <- newPoints) {
      newPoint.iter = iterationNumber
      val newTriplet = MOGENTriplet(newPoint, currentTriplet.getAlgorithm, currentTriplet.getAlgorithmState.getNewState(newPoint))
      archiveChanged = archiveChanged || archive.contains(newTriplet)
    }
    archive.insert(currentTriplet)
    if (archiveChanged) MOGEN.onArchiveChanged(archive)
  }

  def selectIterate: MOGENTriplet = {
    val newIterate = selectionHeuristic(archive)
    newIterate
  }

}

object MOGEN {

  def apply(evaluator: MOEvaluator, visu: Boolean = false) = new MOGEN(evaluator)

  def fairSelect[E](archive: ParetoFront[Double, MOGENTriplet]): MOGENTriplet = {
    archive.priorityQueue.dequeue match {
      case triplet: MOGENTriplet => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
  
  def randomSelect[E](archive: ParetoFront[Double, MOGENTriplet]): MOGENTriplet = {
    archive.randomElement match {
      case triplet: MOGENTriplet => triplet
      case _ => throw new IllegalArgumentException("A MOGEN archive must only contain MOGEN triplet")
    }
  }
  
  var onIterateSelected: (MOGENTriplet) => Unit = {triplet: MOGENTriplet => }
  var onArchiveChanged: (ParetoFront[_, _]) => Unit = {newArchive: ParetoFront[_, _] => }
}
