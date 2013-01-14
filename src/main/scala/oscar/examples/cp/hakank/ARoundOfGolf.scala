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

import oscar.cp.core._


/**
 *
 * A Round of Golf puzzle (Dell Logic Puzzles) in Oscar.
 *
 * From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
 * """
 * Title: A Round of Golf
 * Author: Ellen K. Rodehorst
 * Publication: Dell Favorite Logic Problems
 * Issue: Summer, 2000
 * Puzzle #: 9
 * Stars: 1
 *
 * When the Sunny Hills Country Club golf course isn't in use by club members,
 * of course, it's open to the club's employees. Recently, Jack and three other
 * workers at the golf course got together on their day off to play a round of
 * eighteen holes of golf.
 * Afterward, all four, including Mr. Green, went to the clubhouse to total
 * their scorecards. Each man works at a different job (one is a short-order
 * cook), and each shot a different score in the game. No one scored below
 * 70 or above 85 strokes. From the clues below, can you discover each man's
 * full name, job and golf score?
 *
 * 1. Bill, who is not the maintenance man, plays golf often and had the lowest
 * score of the foursome.
 * 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten
 * strokes more than the pro-shop clerk.
 * 3. In some order, Frank and the caddy scored four and seven more strokes than
 * Mr. Sands.
 * 4. Mr. Carter thought his score of 78 was one of his better games, even
 *    though Frank's score  was lower.
 * 5. None of the four scored exactly 81 strokes.
 *
 * Determine: First Name - Last Name - Job - Score
 * """
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ARoundOfGolf {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 4

    val Jack  = 0
    val Bill  = 1
    val Paul  = 2
    val Frank = 3

    // variables
    val last_name = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(clubb, sands, carter, green) = last_name

    val job = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(cook, maintenance_man, clerk, caddy) = job

    val score = Array.fill(n)(CPVarInt(cp, 70 to 85))


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(allDifferent(last_name), Strong);
      cp.add(allDifferent(job), Strong);
      cp.add(allDifferent(score), Strong);

      // 1. Bill, who is not the maintenance man, plays golf often and had
      //    the lowest score of the foursome.
      cp.add(maintenance_man != Bill);
      cp.add(score(Bill) < score(Jack));
      cp.add(score(Bill) < score(Paul));
      cp.add(score(Bill) < score(Frank));
      
      // 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and
      //    scored ten strokes more than the pro-shop clerk.
      cp.add(clubb != Paul);
      cp.add(score(clubb) == score(clerk) + 10);
      
      // 3. In some order, Frank and the caddy scored four and seven more
      //    strokes than Mr. Sands.
      cp.add(caddy != Frank);
      cp.add(sands != Frank);
      cp.add(caddy != sands);

      cp.add(
             (
              (score(Frank) === score(sands) + 4) &&
              (score(caddy) === score(sands) + 7 )
              )
             ||
             (
              (score(Frank) === score(sands) + 7)   &&
              (score(caddy) === score(sands) + 4)
              )
             )


      // 4. Mr. Carter thought his score of 78 was one of his better games,
      //    even though Frank's score was lower.
      cp.add(carter != Frank);
      cp.add(score(carter) == 78);
      cp.add(score(Frank) < score(carter));
      
      // 5. None of the four scored exactly 81 strokes.
      for(i <- 0 until n) {
        cp.add(score(i) != 81);
      }


     } exploration {
       
       cp.binaryFirstFail(last_name ++ job ++ score)

       println("last_name:" + last_name.mkString(""))
       println("job      :" + job.mkString(""))
       println("score    :" + score.mkString(""))
       println()

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
