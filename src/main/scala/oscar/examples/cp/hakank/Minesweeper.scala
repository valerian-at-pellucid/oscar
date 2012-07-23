/*******************************************************************************
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
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._

import scala.io.Source._

/**
 *
 * Minesweeper in Oscar.
 *
 * From gecode/examples/minesweeper.cc:
 * """
 *  A specification is a square matrix of characters. Alphanumeric characters represent
 *  the number of mines adjacent to that field. Dots represent fields with an unknown number
 *  of mines adjacent to it (or an actual mine).
 *  """
 *
 *   E.g.
 *   "..2.3."
 *   "2....."
 *   "..24.3"
 *   1.34.."
 *   ".....3"
 *   ".3.3.."
 *   """
 *  
 *  Also see 
 *   
 *  http://www.janko.at/Raetsel/Minesweeper/index.htm
 *  (the first 10 examples are from)
 * 
 *  http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
 *
 *  Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/
 *
 *  Richard Kaye's Minesweeper Pages
 *  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
 *  Some Minesweeper Configurations
 *  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Minesweeper extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    //
    // Oleg German, Evgeny Lakshtanov: "Minesweeper" without a computer
    //  http://arxiv.org/abs/0806.3480, page 4
    // data
    var r = 5
    var c = 6
    var X = -1
      
    var game = List(List(X,1,X,1,X,1),
                    List(2,X,2,X,1,X),
                    List(X,3,X,2,X,1),
                    List(1,X,3,X,2,X),
                    List(X,1,X,2,X,1))

    // read from file
    if (args.length > 0) {
      println("Read from file: " + args(0))
    
      val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
      r = lines(0).toInt
      c = lines(1).toInt
      println("r:" + r)
      println("c:" + c)
      var this_game = List[List[Int]]()
      for (t <- 2 to r+1) {
        println(lines(t))
        val t2:List[Int] = lines(t).split("").tail.toList.map(i=>if (i == ".") X else i.toInt)
        this_game = this_game ::: List(t2)
      }
      game = this_game
    }


    // variables
    val mines = Array.fill(r)(Array.fill(c)(CPVarInt(cp, 0 to 1)))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      val tmp = List(-1,0,1)

      for(i <- 0 until r) {
        for(j <- 0 until c if game(i)(j) >= 0) {

          cp.add(sum( for{ a <- tmp; b <- tmp 
                      if ( (i+a >= 0) && (j+b  >= 0) &&
                           (i+a <  r)  && (j+b <  c)) 
                        } yield mines(i+a)(j+b)) == game(i)(j))
            
          // redundant constraint
          if (game(i)(j) > X) {
            cp.add(mines(i)(j) == 0)
          }
        }
      }


    } exploration {
       
      cp.binaryFirstFail(mines.flatten)

      println("\nSolution:")
      for(i <- 0 until r) {
        println(mines(i).mkString(""))
      }

      numSols += 1
       
     }

     println("\nIt was " + numSols + " solutions.\n")
     cp.printStats()

   }

}
