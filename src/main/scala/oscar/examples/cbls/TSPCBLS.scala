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
 ******************************************************************************/

package oscar.examples.cbls

import oscar.cbls.search.StopWatch
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.Model
import util.Random
import scala.math._
import oscar.cbls.routing.model._
import oscar.cbls.routing.initialSolution._
import oscar.cbls.routing.neighborhood._

/**supports only a single vehicle*/
object TSPCBLS extends SearchEngine with StopWatch with App{

  // tester si l'objectif est bien la distance réel via la matrice de base des distances.
  def realSum(vrp:VRP,matrice:Array[Array[Int]])={
    var sum:Int = 0
    for(i<- 0 until vrp.Next.length){
      val j = vrp.Next(i).value

      sum += matrice(i)(j)
    }
    sum
  }
  val N:Int = 1000

  this.startWatch()

  println("TSP(" + N + ")")
  val random = new Random(0)
  
  def getRandomDistanceMatrix(N:Int):Array[Array[Int]]= Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else random.nextInt(1000)+1)

  def getPlanarDistanceMatrix(N:Int):Array[Array[Int]] = {
    val coordX = Array.tabulate(N)(_ => random.nextInt(1000))
    val coordY = Array.tabulate(N)(_ => random.nextInt(1000))
    Array.tabulate(N,N)((i,j) => round(sqrt((   pow(coordX(i) - coordX(j), 2)
                   + pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt )
  }

  val DistanceMatrix = getPlanarDistanceMatrix(N)

  val m: Model = new Model(false,None,false,false)
  val vrp = new VRP(N, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
  /*with SymmetricVRP*/ with Unrouted /*with Predecessors with OtherFunctionToObjective with WeakConstraints
    with StrongConstraints*/
  vrp.installCostMatrix(DistanceMatrix)
  vrp.saveKNearestPoints(20)

/*
  val strongConstraintSystem = new ConstraintSystem(m)

  val weakConstraintSystem = new ConstraintSystem(m)
  val maxNodes = 20
  val minNodes = 10
  val strongPenalty = new IntVar(m,Int.MinValue,Int.MaxValue,1000,"StrongC. penality")
  val weakPenalty = new IntVar(m,Int.MinValue,Int.MaxValue,1000,"WeakC. penality")

  for(i <- 0 until vrp.V){
    strongConstraintSystem.post(LE(vrp.RouteLength(i),new IntVar(m,0,N,maxNodes,"max node in route "+i)),strongPenalty)
    strongConstraintSystem.violation(vrp.RouteLength(i))
  }

  for(i <- 0 until vrp.V){
    weakConstraintSystem.post(GE(vrp.RouteLength(i),new IntVar(m,0,N,minNodes,"max node in route "+i)),weakPenalty)
    weakConstraintSystem.violation(vrp.RouteLength(i))
  }
  val withConstraints = false
  if(withConstraints)
    vrp.setStrongConstraints(strongConstraintSystem)
  if (withConstraints)
    vrp.setWeakConstraints(weakConstraintSystem)


  strongConstraintSystem.close()
  weakConstraintSystem.close()
  //vrp.recordAddedFunctions(Array(strongConstraintSystem.violation,weakConstraintSystem.violation))

*/
  m.close()

  println("closed " + getWatchString)
  NearestNeighbor(vrp)
  //RandomNeighbor(vrp)
  m.propagate()

  println("start val: " + vrp.ObjectiveVar.value)
  println(vrp)
  //println(vrp.routes)
  var nsize = 20
  // 20 for the ThreeOpt withtout reverse
  // 20-100 for the ThreeOpt with 1 or 2 reverse works well

  var Neighbor = false
  var move:Neighbor = null
  var saturated = false
  var it = 0
  while(!saturated){
    val oldobj:Int = vrp.ObjectiveVar.value
    move = OnePointMove.getFirstImprovingMove(vrp,vrp.getKNearest(nsize),move)
    //move = ThreeOptA.getFirstImprovingMove(vrp, vrp.getKNearest(nsize), move)
    //move = ThreeOptC.getFirstImprovingMove(vrp, vrp.getKNearest(nsize), move)
    //move = ThreeOptB.getFirstImprovingMove(vrp, vrp.getKNearest(nsize), move)
    if (move != null && move.getObjAfter < oldobj){
      it +=1
      move.comit
      vrp.ObjectiveVar.value
      if(it % 100 == 0) println("it: " + it + " " + move + " " + vrp.ObjectiveVar.value + ": " + getWatch)

    }
    else{
        saturated = true
        println("done " + getWatchString + " it: " + it)
    }
  }

  println(vrp.ObjectiveVar.value)
}
