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
package oscar.dfo.multiobjective.mogen.algos.states

import oscar.dfo.utils._
import oscar.util.RandomGenerator
import org.omg.PortableServer.POA

trait Simplex[E] {

  val simplex: Array[MOOPoint]
  
  def simplexSize = simplex.length
  
  var bestPoint: MOOPoint

  def getBestPoint = bestPoint
  
  def nCoordinates = simplex(0).nCoordinates
  
  def worstPoint = simplex(simplexSize - 1)

  def orderSimplex() = {
    val points = simplex.sortWith((point1, point2) => ((point1 == bestPoint) || (point1.dominance(point2) == 0 && 0.5 > RandomGenerator.nextDouble) || point1.dominance(point2) > 0))
    for (i <- 0 until simplexSize) {
      simplex(i) = points(i)
    }
  }

  def getPoints: List[MOOPoint] = simplex.toList

  def arraySum(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) + ar2(i))
  
  def arrayDiff(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) - ar2(i))
  
  def arrayProd(ar: Array[Double], factor: Double): Array[Double] = Array.tabulate(ar.length)(i => factor * ar(i))
  
  def getSinglePointTransformation(centroid: Array[Double], factor: Double, evaluator: MOEvaluator, feasibleReg: FeasibleRegion): MOOPoint = {
    val newCoordinates = arraySum(centroid, arrayProd(arrayDiff(centroid, worstPoint.coordinates), factor))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def applySinglePointTransformation(newPoint: MOOPoint) = {
    simplex(simplexSize - 1) = newPoint
    orderSimplex()
  }
  
  def getMultiPointTransformation(factor: Double, evaluator: MOEvaluator, feasibleReg: FeasibleRegion): Array[MOOPoint] = {
    Array.tabulate(simplex.length){i =>
      if (i == 0) bestPoint.clone()
      else evaluator.eval(arrayDiff(bestPoint.coordinates, arrayProd(arrayDiff(simplex(i).coordinates, bestPoint.coordinates), factor)), feasibleReg)
    }
  }
  
  def applyMultiPointTransformation(newSimplex: Array[MOOPoint]) {
    for (i <- 0 until simplex.length)
      simplex(i) = newSimplex(i)
    orderSimplex()
  }
  
  def getCentroid: Array[Double] = {
    val allButWorstCoordinates = simplex.map(mooP => mooP.coordinates).take(simplexSize - 1)
    arrayProd(allButWorstCoordinates.drop(1).foldLeft(allButWorstCoordinates(0))((acc, newCoords) => arraySum(acc, newCoords)), 1.0 / (simplexSize - 1))
  }
  
  def getValidCoordinate(index: Int, intervals: Array[(Double, Double)]): Double = {
    val randPerturb = (0.5 - RandomGenerator.nextDouble) * math.abs(intervals(index)._2 - intervals(index)._1)
    val newCoord = bestPoint.coordinates(index) + randPerturb
    if (newCoord >= intervals(index)._1 && newCoord <= intervals(index)._2) newCoord
    else getValidCoordinate(index, intervals)
  }
  
  def reinitializeSimplex(intervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): Unit = {
    val newCoords = Array.tabulate(simplexSize){ index =>
      Array.tabulate(bestPoint.coordinates.length)(i => getValidCoordinate(i, intervals))
    }
    for (i <- 1 until simplexSize)
      simplex(i) = evaluator.eval(newCoords(i), feasReg)
  }
  
  def getSmallestEdge: Double = {
    val coords = simplex.map(point =>point.coordinates)
    var minDist = Double.MaxValue
    for (i <- 0 until simplex.length) {
      for (j <- (i + 1) until simplex.length) {
        minDist = math.min(minDist, euclidianDistance(coords(i), coords(j)))
      }
    }
    minDist
  }
  
  def euclidianDistance(c1: Array[Double], c2: Array[Double]): Double = {
    var sum = 0.0
    for (i <- 0 until c1.length) {
      sum += math.pow(c2(i) - c1(i), 2.0)
    }
    math.sqrt(sum)
  }
}