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

/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.numeric

import oscar.cbls.invariants.core.computation._;

object Implicits{

  implicit def InstrumentIntVar(v:IntVar):InstrumentedIntVar = new InstrumentedIntVar(v)
  implicit def InstrumentIntInvariant(i:IntInvariant):InstrumentedIntVar = InstrumentIntVar(i.toIntVar)
  implicit def InstrumentInt(a:Int):InstrumentedIntVar = InstrumentIntVar(IntConst(a))

  class InstrumentedIntVar(x:IntVar){
    def plus (v:IntVar):IntInvariant = Sum2(x,v)

    def minus (v:IntVar):IntInvariant = Minus(x,v)
    def times (v:IntVar):IntInvariant = Prod(List(x,v))

    def div (v:IntVar):IntInvariant = Div(x,v)
  }
}
