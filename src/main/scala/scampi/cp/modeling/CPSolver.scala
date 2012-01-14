/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.modeling

import scampi.search._;
import scampi.cp._
import scampi.cp.core._;
import scampi.cp.search._;
import scampi.cp.constraints._;

class NoSol(msg : String) extends Exception(msg)

class CPSolver() extends Store() { 
	
	val branchings = new BranchingCombinator()
	
	var search = new Explorer(this, branchings)
	
	var startSearch : Unit => Unit = null
	
	var time : Long = 0
	
	var tree = false
	
	/**
	 * @param block a code block
	 * @return the time (ms) to execute the block
	 */
	def getTime(block: => Unit): Long = {
			val t0 = System.currentTimeMillis()
			block
			System.currentTimeMillis - t0
	}
	
	def showTree() {
		search = new Explorer(this, branchings)
	}
	
	def += (cons : Constraint,propagStrength: CPPropagStrength = CPPropagStrength.Weak ) : Unit = {
		 this.add(cons, propagStrength)
	}

	
	def minimize(obj : CPVarInt) : CPSolver = {
	    minimization(obj)
		startSearch = { Unit => search.minimize(obj) } 
		this
	}
	
	def maximize(obj : CPVarInt) : CPSolver = {
		maximization(obj)
		startSearch = { Unit => search.maximize(obj) } 
		this
	}
	
	def solve() : CPSolver = {
		startSearch = { Unit =>
      println("start search...")
      search.findOneSolution }
		this
	}
	
	def solveAll() : CPSolver = {
		startSearch = { Unit => search.findAllSolutions } 
		this
	}
	
	def subjectTo(constraintsBlock : => Unit) : CPSolver = {
		try {
			constraintsBlock
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model")
		}
		this
	}
	
	def addBranching(searchBlock : Branching) {
		branchings.addBranching(searchBlock)
	}
	
	def addBranching(searchBlock : => Array[Alternative] ) {
		branchings.addBranching(
						new Branching() {
							override def getAlternatives(): Array[Alternative] = {
									searchBlock
							}
						})
	}
	
	def exploring(searchBlock : => Array[Alternative]) {
		if (this.getStatus() != CPOutcome.Failure) {
			time = getTime {
				branchings.addBranching(
						new Branching() {
							override def getAlternatives(): Array[Alternative] = {
									searchBlock
							}
						})

						time = getTime {
							startSearch()
						}
			}
		}
	}
	
	def exploring(branching : Branching*) : CPSolver = {
		if (this.getStatus() != CPOutcome.Failure) {
			branching.foreach(branchings.addBranching(_))
			time = getTime {startSearch()}
		} else {
          println("failed model")
        }
		this
	}
	
	def exploring(searchBlock : => Unit) : CPSolver = {
		searchBlock //execute the search block adding the branchings
		if (this.getStatus() != CPOutcome.Failure) {
			time = getTime {startSearch()}
		}
		this
	}
	/*
	def relaxAfter(nbFail : Int)(block: => Unit ) : CPSolver = {
		search.relaxAfter(nbFail)(block)
		this
	}*/
	
//	def relaxAfter(block: => Unit ) : CPSolver = {
//		search.relaxAfter(200)(block)
//		this
//	}	

	def onSolution(block: => Unit ) : CPSolver = {
		search.onSolution(block)
		this
	}
	
	def printStats() {
		println("time(ms)",time)
		println("#bkts",search.getNbBkts())
		println( "time in fix point(ms)",getTimeInFixPoint())
		println( "time in trail restore(ms)",getTrail().getTimeInRestore())
		println( "max trail size",getTrail().getMaxSize())
	}
	
	def branchOn(vals : Range,filter : (Int => Boolean), by : (Int => Double) ) (cons: (Int => Constraint)) : Array[Alternative] = {
		val alts = for(v <- vals; if filter(v))  yield (by(v),new CPAlternative(this,cons(v)))
		val sortedalts = alts.toList.sort((e1,e2) => (e1._1 < e2._1))
		sortedalts.map(_._2).toArray
	}
	
	def branchOn(x : CPVarInt, by : (Int => Double) = (v => v) ) (cons: (Int => Constraint)) : Array[Alternative] = {
		branchOn(x.getMin to x.getMax,x.hasValue(_),by)(cons)
	}
	
	
	def branchOn(c : Constraint*) : Array[Alternative] = {
		c.map(cons => new CPAlternative(this,cons)).toArray
	}
}

object CPSolver {

  /**
   * Creates a new CP Solver
   */
  def apply(): CPSolver = {
    new CPSolver()
  }
}

