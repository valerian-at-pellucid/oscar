/**
 * *****************************************************************************
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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._


/**
 * Balanced Academic Curriculum Problem
 * The BACP is to design a balanced academic curriculum by assigning periods to courses in a way that 
 * the academic load of each period is balanced, i.e., as similar as possible . The curriculum must obey the following administrative and academic regulations: 
 * Academic curriculum: an academic curriculum is defined by a set of courses and a set of prerequisite relationships among them. 
 * Number of periods: courses must be assigned within a maximum number of academic periods. 
 * Academic load: each course has associated a number of credits or units that represent the academic effort required to successfully follow it. 
 * Prerequisites: some courses can have other courses as prerequisites. 
 * Minimum academic load: a minimum amount of academic credits per period is required to consider a student as full time. 
 * Maximum academic load: a maximum amount of academic credits per period is allowed in order to avoid overload. 
 * Minimum number of courses: a minimum number of courses per period is required to consider a student as full time. 
 * Maximum number of courses: a maximum number of courses per period is allowed in order to avoid overload. 
 * The goal is to assign a period to every course in a way that 
 * - the minimum and maximum academic load for each period, 
 * - the minimum and maximum number of courses for each period, 
 * - and the prerequisite relationships are satisfied. 
 * An optimal balanced curriculum balances academic load for all periods. 
 * @author Pierre Schaus pschaus@gmail.com
 */
object BACP extends App{

    val lines = Source.fromFile("data/bacp/instances12/inst0.txt").getLines.reduceLeft(_ + " " + _)
    val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
    var index = 0
    def next() = {
      index += 1
      vals(index - 1)
    }

    val nbCourses = next()
    val courses = 0 until nbCourses
    val nbPeriods = next()
    val periods = 0 until nbPeriods
    val mincredit = next()
    val maxcredit = next()
    val nbPre = next()
    val credits = Array.fill(nbCourses)(next())
    val prerequisites = Array.fill(nbPre)((next(), next()))

    val cp = CPSolver()
    var x = Array.fill(nbCourses)(CPVarInt(cp,periods))
    val l = Array.fill(nbPeriods)(CPVarInt(cp,0 to credits.sum))
    val vari = CPVarInt(cp,0 to 10000000)



    cp.minimize(vari) subjectTo {
        cp.add(spread(l,credits.sum, vari))
        cp.add(binPacking(x, credits, l))
        for ((i,j) <- prerequisites) {
          cp.add(x(i) < x(j)) // precedence constraint
        } 
    } exploration {
        cp.binaryFirstFail(x,x => selectMin(periods)(x.hasValue(_))(l(_).min).get)
    } run()
    
    cp.printStats

}
