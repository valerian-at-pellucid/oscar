/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples

import scampi.linprog.modeling._

/**
 * @author Pierre Schaus pschaus@gmail.com
 * Cutting Stock using Column Generation
 */
object CuttingStock extends LPModel with MIPModel {
	
  class Column (val x : LPVar, val shelf : Array[Int]) {
	  override def toString() : String = {
	 	  shelf.mkString("\t")
	  }   
	  def number() : Int = Math.ceil(x.getValue).toInt
  }
  
  
  def main(args: Array[String]) {
	  	  
	  val boardWidth = 110
	  val nbShelves = 5
	  val Shelves = 0 until nbShelves
	  val shelf =  Array(20, 45, 50, 55, 75)
	  val demand = Array(48, 35, 24, 10,  8)

	  val lp = LPSolver(LPSolverLib.lp_solve)
	  var C : Array[Column] = Array()
	  for (s <- Shelves) {
	 	  val config = Array.tabulate(nbShelves)(_ => 0)
	 	  config(s) = boardWidth/shelf(s)
	 	  C = C :+ new Column(new LPVar(lp,"config"+s), config)
	  }
	   
	  var meet = Array[LPConstraint]()
 
	  // Master Problem 
	  lp.minimize(sum(C)(c => c.x)) subjectTo {
	 	  for (s <- Shelves) {
	 	 	  meet = meet :+ lp.add(sum(C)(c => c.x * c.shelf(s)) >= demand(s))
	 	  }
	  }
	  println("master obj:" + lp.getObjectiveValue)
	  
	  // Pricing Problem
	  var mip : MIPSolver = null
	  do {
		  mip = MIPSolver()
		  val use = Array.tabulate(nbShelves)(_ => MIPVar(mip,"use",0 to boardWidth))
		  val cost = Array.tabulate(nbShelves)(meet(_).getDual)

		  mip.minimize(1 - sum(Shelves)(s => cost(s) * use(s))) subjectTo {
			  mip.add(sum(Shelves)(s => shelf(s)*use(s)) <= boardWidth)
		  }

		  val x = lp.addColumn(1,meet toArray,use.map(_.getValue)) //create a new variable by introducing a new column
		  
		  C = C :+ new Column(x, use.map(_.getValue.toInt))		  
		  
		  println("master obj:" + lp.getObjectiveValue)
		  

	  } while(mip.getObjectiveValue < 0)

	  	  
	  println("\n"+shelf.mkString("\t"))
	  println("-----------------------------------")
	  C.foreach(c => println(c+" * "+c.number))
	  println("-----------------------------------")
	  println("total #boards:" + C.map(_.number).sum)
	  
  }
}
