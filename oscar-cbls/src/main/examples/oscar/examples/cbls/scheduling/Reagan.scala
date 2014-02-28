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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.examples.cbls.scheduling

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.scheduling._
import oscar.cbls.scheduling.algo.{IFlatIRelaxTabu, IFlatIRelax}
import oscar.cbls.scheduling.model._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.scheduling.model.CumulativeResource

/**a simple model of Reagan president of USA
 * he is partly multitask, can do two things at the same time, except eating, which requires his full attention
 * he needs to sleep 2H, eat 30', chew 45' and think 3H
 * he cannot sleep before having eaten
 */
object Reagan extends App {
  val model = new Store(verbose=false, checker = None, noCycle=false, topologicalSort = false)

  val planning = new Planning(model, 50)
  val solver = new IFlatIRelaxTabu(planning)

  val Reagan = CumulativeResource(planning, 3, "Reagan")

  val Eat = Activity(2, planning, "eat")
  Eat uses 2 ofResource Reagan

  val Sleep = Activity(8, planning, "sleep")
  Sleep uses 1 ofResource Reagan

  val Think = Activity(12, planning, "think")
  Think uses 1 ofResource Reagan

  val Chew = Activity(3, planning, "chew")
  Chew uses 1 ofResource Reagan

  val Speak = Activity(3, planning, "speak")
  Speak uses 3 ofResource Reagan

  val Drink = Activity(3, planning, "drink")
  Drink uses 3 ofResource Reagan

  val Pray = NonMoveableActivity(5, 2, planning, "pray")
  Pray uses 2 ofResource Reagan

  val Digest = SuperActivity(Eat, Sleep, "digest")
  Digest uses 1 ofResource Reagan

  Think precedes Drink
  Eat precedes Sleep
  Chew precedes Speak

  model.close(false)


  solver.Solve(MaxIt = 100,
            Stable = 50,
            NbRelax = 5,
            PkillPerRelax = 10,
            tenure = 5,
            jumpAfterPlateau = 7)

  println(planning.toAsciiArt)
  println(planning.resourceUsage)
  println(planning.dependencies)

  /*
  digest              :[0   ;15  ] #=============#
  eat                 :[0   ;2   ] ##
  chew                :[2   ;5   ]   #=#
  pray                :[5   ;7   ]      ##
  think               :[7   ;19  ]        #==========#
  sleep               :[7   ;15  ]        #======#
  speak               :[19  ;22  ]                    #=#
  drink               :[22  ;25  ]                       #=#
  MakeSpan:=25

  Reagan               |3        | ++   ++++++++++    ++++++
                       |2        | +++++++++++++++    ++++++
                       |1        | +++++++++++++++++++++++++
  */
}

