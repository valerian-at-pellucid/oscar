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

  Zebra problem in Oscar.

  """
  This is the zebra problem as invented by Lewis Caroll.

  There are five houses.
  The Englishman lives in the red house.
  The Spaniard owns the dog.
  Coffee is drunk in the green house.
  The Ukrainian drinks tea.
  The green house is immediately to the right of the ivory house.
  The Old Gold smoker owns snails.
  Kools are smoked in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian lives in the first house.
  The man who smokes Chesterfields lives in the house next to the man
    with the fox.
  Kools are smoked in the house next to the house where the horse is kept.
  The Lucky Strike smoker drinks orange juice.
  The Japanese smokes Parliaments.
  The Norwegian lives next to the blue house.

  Who owns a zebra and who drinks water?
  """

  This is a slightly alternative version of 
     http://www.hakank.org/oscar/Zebra.scala
  The difference is mostly how the decision variables are declared
  which makes it a little neater model.
  

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Zebra2 extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    //
    // variables
    // 
    val colors      = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(red, green, yellow, blue, ivory) = colors

    val nationality = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(englishman,spaniard,japanese,ukrainian,norwegian) = nationality

    val animal      = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(dog,snails,fox,zebra,horse) = animal

    val drink       = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(tea,coffee,water,milk,fruit_juice) = drink

    val smoke       = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(old_gold,kools,chesterfields,lucky_strike,parliaments) = smoke

    // for labeling
    val all_vars = colors ++ nationality ++ animal ++ drink ++ smoke

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {
    
       cp.add(alldifferent(colors), Strong)
       cp.add(alldifferent(nationality), Strong)
       cp.add(alldifferent(animal),Strong)
       cp.add(alldifferent(drink), Strong)
       cp.add(alldifferent(smoke), Strong)

       // The clues
       cp.add(englishman == red)
       cp.add(spaniard == dog)
       cp.add(coffee == green)
       cp.add(ukrainian == tea)
       cp.add(green == ivory + 1)
       cp.add(old_gold == snails)
       cp.add(kools == yellow)
       cp.add(milk == 3)
       cp.add(norwegian == 1)
       cp.add((fox - chesterfields).abs() == 1)
       cp.add((horse - kools).abs() == 1)
       cp.add(lucky_strike == fruit_juice)
       cp.add(japanese == parliaments)
       cp.add((norwegian - blue).abs() == 1)



    } exploration {
       
      // cp.binary(all_vars)
      cp.binaryFirstFail(all_vars)
      // cp.binaryMaxDegree(all_vars)

      println("\nSolution:")

      val ns = Array("englishman", "spaniard", "japanese", "ukrainian", "norwegian")

      println("water drinker: " + 
              ns((for{i <- 0 until n if nationality(i).getValue() == water.getValue()} yield i).head))
      println("owns zebra: " + 
              ns((for{i <- 0 until n if nationality(i).getValue() == zebra.getValue()} yield i).head))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
