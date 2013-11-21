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
package oscar.algebra

class Const(val d: Double) extends LinearExpression {

  val cte = d
  val coef = scala.collection.immutable.Map[Var, Double]()

  def *(expr: LinearExpression): LinearExpression = new LinearExpressionProd(this, expr)

  def *(c2: Const) = new Const(d * c2.d)

  def +(c2: Const) = new Const(d + c2.d)

  def -(c2: Const) = new Const(d - c2.d)

  def *(x: Var) = new CstVar(this, x)

  override def toString = d.toString

  override def derive(v: Var): Expression = Zero

}

object Const {
  def apply(d: Double): Const = d match {
    case 0.0 => Zero
    case 1.0 => One
    case _ => new Const(d)
  }
  def unapply(c: Const): Option[Double] = Some(c.d)
}  
