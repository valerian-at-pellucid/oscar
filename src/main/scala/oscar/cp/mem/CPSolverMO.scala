/** *****************************************************************************
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
  * ****************************************************************************
  */

package oscar.cp.mem

import oscar.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.util.continuations._
import scala.collection.mutable.Stack
import oscar.cp.scheduling.CumulativeActivity
import oscar.reversible._
import oscar.cp.mem.ParetoFront.ParetoSet
import oscar.cp.mem.ParetoFront.ParetoPoint

class NoSol(msg: String) extends Exception(msg)

class CPSolverMO(val nObjs : Int) extends Store() {
	
	// Approximation of the pareto set
	val paretoSet    : ParetoSet   = ParetoSet(nObjs)
	var currentPoint : ParetoPoint = null
	
	var solutionVector : Array[CPVarInt] = null
	def setSolutionVector(sol : Array[CPVarInt]) { solutionVector = sol }
	def buildSolution : Array[Int] = solutionVector.map(v => v.value)
	
	var objProcessed = 0

	// DEFINE THE MO LNS FRAME WORK
	// -------------------------------------------------

	// Relaxation factor
	val e = 0.2

	case class MOLNS(nbFailures: Int, sol: MOSolution, restart: () => Unit)
	var moLns : () => Unit = null

	private var lastLNSRestartCompleted = false

	def moLns(nbFailures: Int, nbRestart : Int)(restart: => Unit) {
		failLimit = nbFailures	
		//maxRestart = nbRestart
		moLns = () => restart
	}

	/** Select the next point to process
	  */
	def selectPoint: ParetoPoint = null

	/** Select next objective to improve
	  */
	def selectObjective = (objective.currentObjectiveIdx + 1) % nObjs

	def objRelax(p : ParetoPoint) = {

		for (i <- 0 until nObjs) {
			if (i == objective.currentObjectiveIdx) {

				// Current objective must be improved
				objective.bounds(i) = p(i)
			}
			else {

				// Relax other objectives e
				objective.bounds(i) = ((1+e) * p(i)).toInt
			}
		}
	}

	/** @return true if the last LNS restart was caused because of completed exploration of search tree,
	  * false otherwise (i.e. limit on the number failure reached)
	  */
	def isLastLNSRestartCompleted = lastLNSRestartCompleted

	// -------------------------------------------------

	/** @param block a code block
	  * @return the time (ms) to execute the block
	  */
	def getTime(block: => Unit): Long = {
		val t0 = System.currentTimeMillis()
		block
		System.currentTimeMillis - t0
	}

	def +=(cons: Constraint, propagStrength: CPPropagStrength = CPPropagStrength.Weak): Unit = {
		this.add(cons, propagStrength)
	}

	var stateObjective: Unit => Unit = Unit => Unit

	//(obj1,weight1,id1,"name1"),(obj2,weight2,id2,"name2")
	
	def minimize(objectives: CPVarInt*): CPSolverMO = {
		stateObjective = Unit => {
			val o = new CPObjectiveMinimize(objectives: _*)
			objective = o
			post(o)
		}
		solveAll()
		this
	}

	def solve(): CPSolverMO = {
		solveOne = true
		this
	}

	def solveAll(): CPSolverMO = {
		solveOne = false
		this
	}

	def subjectTo(constraintsBlock: => Unit): CPSolverMO = {
		try {
			constraintsBlock
		}
		catch {
			case ex: NoSol => println("No Solution, inconsistent model")
		}
		this
	}

	/** return true if every variable is bound
	  */
	def allBounds(vars: IndexedSeq[CPVarInt]) = vars.map(_.isBound).foldLeft(true)((a, b) => a & b)

	def minDom(x: CPVarInt): Int = x.size
	def minRegre(x: CPVarInt): Int = x.max - x.min
	def minDomMaxDegree(x: CPVarInt): (Int, Int) = (x.size, -x.constraintDegree)
	def minVar(x: CPVarInt): Int = 1
	def maxDegree(x: CPVarInt): Int = -x.constraintDegree

	def minVal(x: CPVarInt): Int = x.min
	def maxVal(x: CPVarInt): Int = x.max
	def minValminVal(x: CPVarInt): (Int, Int) = (x.min, x.min)

	/** Binary First Fail on the decision variables vars
	  */
	def binaryFirstFail(vars: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
		while (!allBounds(vars)) {
			val unbound = vars.filter(!_.isBound)
			val minDomSize = unbound.map(_.size).min
			val x = unbound.filter(_.getSize == minDomSize).first
			val v = valHeuris(x)
			branch(post(x == v))(post(x != v)) // right alternative			
		}
	}

	/** Binary search on the decision variables vars with custom variable/value heuristic
	  */
	def binary(vars: Array[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
		while (!allBounds(vars)) {
			val unbound = vars.filter(!_.isBound)
			val heuris = unbound.map(varHeuris(_)).min
			val x = unbound.filter(varHeuris(_) == heuris).first
			val v = valHeuris(x)
			branch(post(x == v))(post(x != v)) // right alternative
		}
	}

	/**
	  */
	def binaryFirstFail(vars: CPVarInt*): Unit @suspendable = {
		binary(vars.toArray, valHeuris = minVal)
	}

	/** Binary search on the decision variables vars, selecting first the variables having the max number
	  * of propagation methods attached to it.
	  */
	def binaryMaxDegree(vars: Array[CPVarInt]): Unit @suspendable = {
		binary(vars, varHeuris = maxDegree, valHeuris = minVal)
	}

	/** Binary search on the decision variables vars, splitting the domain of the selected variable on the
	  * median of the values (left : <= median, right : > median)
	  */
	def binaryDomainSplit(vars: Array[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (Int => Int) = i => i): Unit @suspendable = {

		while (!allBounds(vars)) {

			val unbound = vars.filter(!_.isBound)
			val heuris = unbound.map(varHeuris(_)).min
			val x = unbound.filter(varHeuris(_) == heuris).first

			val vals = x.toArray.sortBy(valHeuris)
			val median = vals(vals.size / 2)

			branch(post(x <= median))(post(x > median))
		}
	}

	def printStats() {
		println("time(ms)", time)
		println("#bkts", sc.nFail)
		println("time in fix point(ms)", getTimeInFixPoint())
		println("time in trail restore(ms)", getTrail().getTimeInRestore())
		println("max trail size", getTrail().getMaxSize())
	}

	override def exploration(block: => Unit @suspendable): Unit = {

		val t1 = System.currentTimeMillis()

		stateObjective()
		var nbRestart = 0
		var maxRestart = 1

		reset {
			shift { k1: (Unit => Unit) =>
				val b = () => {
					sc.start()
					block
					if (!isFailed()) {
						if (solveOne) {
							sc.reset()
							k1() // exit the exploration block
						}
					}
				}

				def restart(relaxation: Boolean = false) {
					
					popAll()
					pushState()

					// Relaxation part
					if (relaxation) {						
						// Reset the search controller
						sc.reset()					
						// MO LNS Relaxation
						moLns
					}

					if (!isFailed()) {
						
						sc.reset()
						nbRestart += 1
						
						reset {							
							// Exploration
							b()
							
							// Solution found
							if (!isFailed()) {
								
								objective.tighten()
								
								// Build the new point
								val newPoint = ParetoPoint(objective)									
								// Add the point to the set
								paretoSet.add(newPoint)								
								// Process a new point
								currentPoint = selectPoint								
								// Reset the number of considered objectives
								objProcessed = 0
							} 
							
							// No solution
							else {
								
								// Next objective
								objective.currentObjective = selectObjective
								objProcessed = objProcessed + 1
					
								if (objProcessed >= nObjs) {
					
									currentPoint = selectPoint
									objProcessed = 0
								}
							}
						}
						
						// Let's go, unless the user decided to stop
						if (!sc.exit) 
							sc.explore()
					}
				}
				
				// First restart, find a feasible solution so no limit
				restart(false) 
				
				for (r <- 2 to maxRestart; if (!objective.isOptimum() && !sc.exit)) {
					
					// Second (or more) restart : relaxation is allowed
					restart(true)
					
					if (sc.isLimitReached) {
						lastLNSRestartCompleted = false
						print("!")
					}
					else {
						lastLNSRestartCompleted = true
						print("R")
					}
				}
				k1() // exit the exploration block       
			}
		}
		time = System.currentTimeMillis() - t1
	}

}

object CPSolverMO {

	/** Creates a new CP Solver
	  */
	def apply(nObjs : Int): CPSolverMO = {
		new CPSolverMO(nObjs)
	}
}

