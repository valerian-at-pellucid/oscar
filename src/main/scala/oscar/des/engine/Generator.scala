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

package oscar.des.engine

import scala.Math._

import oscar.stochastic._
import scala.util.Random
import scala.util.continuations._
import JSci.maths.statistics._
import oscar.invariants._

object Generator {
	def apply(dist: Distr[Double])(block: => Unit)(implicit m: Model[Any]) = new Generator(m, dist)(block)
	def apply(dist: ProbabilityDistribution) = new NumberGenerator(dist)
}

class Generator(m: Model[Any], var dist: Distr[Double])(block: => Unit) extends Process[Unit]("Generator")(m){

  simulate()

  def restart() ={
    if (!generating) {
      start()
    }
  }
  def start() = {
    generating = true
      while (generating) {
        val t = floor(dist.apply(m)).toInt
        val a = w(m.clock === m.clock().plusMillis(t))
        if (generating){
          block
        }
      }
  }

  var generating = true 
  
  def stop() { generating = false }
  def update(v: Distr[Double]) {
    dist = v
  }
}
class NumberGenerator(dist: ProbabilityDistribution) {

  val generator = new Random()

  var generating = true

  def stop() { generating = false }

  def apply(): Double = dist.inverse(generator.nextDouble)
  def generateNext = apply

}

