/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

 Survo puzzle in Oscar.

 http://en.wikipedia.org/wiki/Survo_Puzzle
 """
 Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
 by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
 Survo system which is a general environment for statistical computing and 
 related areas.
 
 In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
 that each of these numbers appears only once and their row and column sums are 
 equal to integers given on the bottom and the right side of the table. 
 Often some of the integers are given readily in the table in order to 
 guarantee uniqueness of the solution and/or for making the task easier.
 """
 
 See also
 http://www.survo.fi/english/index.html
 http://www.survo.fi/puzzles/index.html
 
 References:
 Mustonen, S. (2006b). "On certain cross sum puzzles"
 http://www.survo.fi/papers/puzzles.pdf 
 Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
 http://www.survo.fi/papers/enum_survo_puzzles.pdf 
 Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
 http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
 R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R
 

 
 @author Hakan Kjellerstrand hakank@gmail.com
 http://www.hakank.org/oscar/
 
*/
object SurvoPuzzle extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    /*
      Default problem
      http://www.survo.fi/puzzles/280708.txt, third puzzle
      Survo puzzle 128/2008 (1700) #364-35846
      
         A  B  C  D  E  F
      1  *  *  *  *  *  * 30
      2  *  * 18  *  *  * 86
      3  *  *  *  *  *  * 55
         22 11 42 32 27 37
    */
    var r = 3
    var c = 6
    var rowsums = List(30, 86, 55)
    var colsums = List(22, 11, 42, 32, 27, 37)
    var problem = List(List(0, 0,  0, 0, 0, 0),
                       List(0, 0, 18, 0, 0, 0),
                       List(0, 0,  0, 0, 0, 0))


    // read problem instance from file
    if (args.length > 0) {
      println("\nReading from file: " + args(0))
      
      val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
      r = lines(0).toInt
      c = lines(1).toInt
      rowsums = lines(2).split(",").toList.map(i=>i.toInt)
      colsums = lines(3).split(",").toList.map(i=>i.toInt)
      println("r:" + r)
      println("c:" + c)
      println("rowsums: " + rowsums)
      println("colsums: " + colsums)
      var this_problem = List[List[Int]]()
      for (t <- 4 to 4+r-1) {
        println(lines(t))
        val t2: List[Int] = lines(t).split(",").toList.map(i=> i.toInt)
        this_problem = this_problem ::: List(t2)
      }
      println()
      problem = this_problem
    }


    // variables
    val x = Array.fill(r)(Array.fill(c)(CPVarInt(cp, 1 to r*c)))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {

      // fill the things we know
      for (i <- 0 until r; j <- 0 until c) {
        if (problem(i)(j) > 0) {
          cp.add(x(i)(j) == problem(i)(j))
        }
      }

      cp.add(alldifferent(x.flatten.toArray), Strong)
  
      // rows and columns
      for (i <- 0 until r) {
        cp.add(sum( Array.tabulate(c)(j=> x(i)(j)) ) == rowsums(i))
      }

      
      for (j <- 0 until c) {
        cp.add(sum( Array.tabulate(r)(i=> x(i)(j)) ) == colsums(j))
      }
      

     } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
       for(i <- 0 until r) {
         for(j <- 0 until c) {
           print(x(i)(j) + " ")
         }
         println()
       }
       println()

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
