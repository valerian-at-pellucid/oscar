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

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{Variable, IntInvariant, IntVar}

/** This is a helper to define an invariant from an Int -> Int function.
 * Ths invariant is not incremental, so it should only be used for very simple functions.
 * it maintains output = fun(a)
 * @param a the parameter of the function
 * @param fun the function to maintain, it is supposed not to listen to any variable in the model
 * @param MyMin the min value of the output
 * @param MyMax the max value of the output
 */
case class IntVar2IntVarFun(a:IntVar, fun:Int => Int, override val MyMin:Int = Int.MinValue, override val MyMax:Int=Int.MaxValue) extends IntInvariant {
  var output:IntVar=null

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  //This method is called by the IntVar when the method <== is applied
  //Example: MyVar <== my_IntInvariant
  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := fun(a)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    assert(v == a)
    output := fun(NewVal)
  }

  override def checkInternals(){
    assert(output.getValue() == fun(a.getValue()))
  }
}

/** This is a helper to define an invariant from an Int x Int -> Int function.
 * Ths invariant is not incremental, so this should only be used for very simple functions.
 * it maintains output = fun(a,b)
 * @param a the first parameter of the function
 * @param b the second parameter of the function
 * @param fun the function to maintain, it is supposed not to listen to any variable in the model
 * @param MyMin the min value of the output
 * @param MyMax the max value of the output
 */
case class IntVarIntVar2IntVarFun(a:IntVar, b:IntVar, fun:((Int, Int) => Int), override val MyMin:Int = Int.MinValue, override val MyMax:Int=Int.MaxValue) extends IntInvariant {

  var output:IntVar=null
  registerStaticAndDynamicDependenciesNoID(a,b)
  finishInitialization()

  override def setOutputVar(v:IntVar){
    output = v
    output.setDefiningInvariant(this)
    output := fun(a,b)
  }

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    output := fun(a,b)
  }

  override def checkInternals(){
    assert(output.getValue() == fun(a.getValue(),b.getValue()))
  }
}
