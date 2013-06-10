/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.core._
import oscar.cp.modeling._

class TestOpposite extends FunSuite with ShouldMatchers {

	test("Test Opposite 1") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Set(0, 1, 2, 3, 4))
		val b = -a
		b.max should be(0)
		b.min should be(-4)
		cp.add(a >= 1)		
		b.max should be(-1)
		cp.add(a <= 3)
		b.min should be(-3)
	}
	

}
