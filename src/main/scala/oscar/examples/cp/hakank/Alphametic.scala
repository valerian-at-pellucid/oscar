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
import scala.math._

/*

  Fairly general (simple) alphametic solver in Oscar.

  Usage:

  scala Alphametic.scala <problem> <base> <start>
                    defaults:
                       problem: "SEND+MORE=MONEY"
                       base   : 10
                       start  : 0   (start digits, normally 0 or 1)
                       


  scala Alphametic.scala
                         ->  solves SEND+MORE=MONEY in base 10

  scala Alphametic.scala  "SEND+MOST=MONEY" 11
                         -> solves SEND+MOST=MONEY in base 11

  scala Alphametic.scala TEST <base>
                         -> solve some test problems in base <base>
                            (defined in test_problems())

  scala Alphametic.scala TEST2
                         -> solve SEND+MORE=MONEY for different bases


  Assumptions:
  - We only solves problems of the form
           NUMBER<1>+NUMBER<2>...+NUMBER<N-1> = NUMBER<N>
    The last number is the sum

  - The words are assumed to be constructed of characters [A-Za-z]+.
    All other characters are considered word delimiters.

  - The acceptable digits are <start> .. <base>-1
    where <start> is a program parameter, default 0
  
  - Leading digits in each word are constrained to be > 0


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Alphametic {

  // 
  //  Sum of the words
  // 
  // wordSum(x, "SEND", base, ht) -> 1000*S + 100*E + 10*N + D
  def wordSum(x: Array[CPVarInt], s: String, base: Int, m: Map[Char,Int]) = {
    val n = s.length
    sum(for(i <- 0 until n) yield x(m(s(i)))*pow(base, n-1-i).toInt)
  }

  //
  // alternative version
  // 
  def wordSum2(x: Array[CPVarInt], s: String, base: Int, m: Map[Char,Int]) : CPVarInt = {
    val n = s.length
    weightedSum(for(i <- 0 until n) yield pow(base, n-1-i).toInt, 
                for(i <- 0 until n) yield x(m(s(i))))
  }


  //
  // Main solve function
  //
  def solve(problem_in: String = "SEND+MORE=MONEY", base: Int = 10, start: Int = 0) {

    val cp = CPSolver()

    //
    // data
    //
    val problem_words = problem_in.split("[^A-Za-z]+")
    val p_len = problem_words.length

    println("\nProblem: " + problem_in + " base: " + base + " start digit: " + start)
    println("problem_words: " + problem_words.mkString(" "))

    // get all unique characters
    val chars = problem_in.filter( _.toString.matches("[a-zA-Z]+")).distinct.sorted
    val n = chars.length
    println("chars: " + chars.mkString(""))
                   
    // create a lookup table: list of (char, index)
    val ht = chars.zip(0 until n).toMap
    println("ht: " + ht)

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, start to base-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(alldifferent(x), Strong)

      // The equation: 
      //    word<0> + word<1> + ... = word<p-1>
      //
      cp.add(
             sum(for(p <- 0 until p_len -1) yield wordSum2(x, problem_words(p), base, ht)) ==
             wordSum2(x, problem_words(p_len-1), base, ht)
             )

      // ensure that all initial digits > 0
      for(p <- 0 until p_len) {
        cp.add( x(ht(problem_words(p)(0))) > 0)
      }

      
    } exploration {
       
      cp.binaryMaxDegree(x)

      println("\nSolution:")
      println("x:" + x.mkString(""))
      val sep = if (base == 10) "" else " ";

      // solution map
      val sol = chars.zip(x.map(_.value)).toMap
      sol.foreach(println)
      println()

      for (p<- 0 until p_len) {
        val e = problem_words(p)
        println(e + ": " + e.map(sol(_)+"").mkString(sep))
      }
      println()

      numSols += 1

    }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()
    
  }

  def testProblems(base: Int, start: Int = 0) {

    val problems = Array(
                         "SEND+MORE=MONEY",
                         "SEND+MOST=MONEY",
                         "VINGT+CINQ+CINQ=TRENTE",
                         "EIN+EIN+EIN+EIN=VIER",
                         "DONALD+GERALD=ROBERT",
                         "SATURN+URANUS+NEPTUNE+PLUTO=PLANETS",
                         "WRONG+WRONG=RIGHT"
                         )

    for (p<-problems) {
      try {
        solve(p, base, start)
      } catch {
        case e: Exception => println("Sorry, couldn't solve this one...")
                             println("Error: " + e + "\n")
      }
    }


  }

  //
  // Testing SEND+MORE=MONEY in different bases
  //
  def testProblems2(problem: String = "SEND+MORE=MONEY", start: Int = 0) {

    for (base <- 1 to 20) {
      try {
        solve(problem, base, start)
      } catch {
        case e: Exception => println("Sorry, couldn't solve this one...")
                             println("Error: " + e + "\n")
      }
    }

  }


  def main(args: Array[String]) {

    val problem = if (args.length > 0) args(0) else "SEND+MORE=MONEY";
    val base    = if (args.length > 1) args(1).toInt else 10;
    val start   = if (args.length > 2) args(2).toInt else 0;

    problem match {
    case "TEST"  | "test"  => testProblems(base, start)
    case "TEST2" | "test2" => testProblems2(start=start)
    case _ => try {
                solve(problem, base, start)
              } catch {
                case e: Exception => println("\nSorry, couldn't solve this one...")
                                     println("Error: " + e + "\n")
              }
    }


  }

}
