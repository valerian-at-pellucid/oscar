/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
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
   

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Zebra extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    //
    // variables
    // 
    // Colors
    val red           = CPVarInt(cp, 1 to n)
    val green         = CPVarInt(cp, 1 to n)
    val yellow        = CPVarInt(cp, 1 to n)
    val blue          = CPVarInt(cp, 1 to n)
    val ivory         = CPVarInt(cp, 1 to n)

    // Nationality
    val englishman    = CPVarInt(cp, 1 to n)
    val spaniard      = CPVarInt(cp, 1 to n)
    val japanese      = CPVarInt(cp, 1 to n)
    val ukrainian     = CPVarInt(cp, 1 to n)
    val norwegian     = CPVarInt(cp, 1 to n)

    // Animal
    val dog           = CPVarInt(cp, 1 to n)
    val snails        = CPVarInt(cp, 1 to n)
    val fox           = CPVarInt(cp, 1 to n)
    val zebra         = CPVarInt(cp, 1 to n)
    val horse         = CPVarInt(cp, 1 to n)

    // Drink
    val tea           = CPVarInt(cp, 1 to n)
    val coffee        = CPVarInt(cp, 1 to n)
    val water         = CPVarInt(cp, 1 to n)
    val milk          = CPVarInt(cp, 1 to n)
    val fruit_juice   = CPVarInt(cp, 1 to n)

    // Smoke
    val old_gold      = CPVarInt(cp, 1 to n)
    val kools         = CPVarInt(cp, 1 to n)
    val chesterfields = CPVarInt(cp, 1 to n)
    val lucky_strike  = CPVarInt(cp, 1 to n)
    val parliaments   = CPVarInt(cp, 1 to n)

    // for labeling
    val all_vars = Array(
                         parliaments, kools, chesterfields, lucky_strike, old_gold,
                         englishman, spaniard, japanese, ukrainian, norwegian,
                         dog, snails, fox, zebra, horse,
                         tea, coffee, water, milk, fruit_juice,
                         red, green, yellow, blue, ivory)

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {
    
       cp.add(alldifferent(Array(red, green, yellow, blue, ivory)), Strong)
       cp.add(alldifferent(Array(englishman, spaniard, japanese, ukrainian, norwegian)), Strong)
       cp.add(alldifferent(Array(dog, snails, fox, zebra, horse)),Strong)
       cp.add(alldifferent(Array(tea, coffee, water, milk, fruit_juice)), Strong)
       cp.add(alldifferent(Array(parliaments, kools, chesterfields, lucky_strike, old_gold)), Strong)


       //
       // The clues
       //
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
       
      cp.binary(all_vars)
      // cp.binaryFirstFail(all_vars)
      // cp.binaryMaxDegree(all_vars)

      println("\nSolution:")

      val p  = Array(englishman, spaniard, japanese, ukrainian, norwegian)
      val ps = Array("englishman", "spaniard", "japanese", "ukrainian", "norwegian")

      println("water drinker: " + ps((for{i <- 0 until n if p(i).getValue() == water.getValue()} yield i).head))
      println("owns zebra: " + ps((for{i <- 0 until n if p(i).getValue() == zebra.getValue()} yield i).head))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
