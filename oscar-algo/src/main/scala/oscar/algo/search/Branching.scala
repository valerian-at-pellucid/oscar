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

package oscar.algo.search

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class Branching {

  /**
   * Initialize the branching
   * For instance in some branching strategy, some statistical probing must be done. This is the place to do it.
   */
  def initialize(): Unit = {}

  /**
   * @return the alternative action to execute in each of the child nodes under the current node
   */
  def alternatives(): Seq[Alternative]

  /**
   * @return the branching resulting of applying this branching, then b if not alternative available
   */
  def ++(b: Branching): Branching = new Branching {
    override def alternatives = {
      val r = Branching.this.alternatives()
      if (r.isEmpty) b.alternatives()
      else r
    }
  }
}

object Branching {
  def apply(b: => Seq[Alternative]): Branching = new Branching() { override def alternatives = b }
}

/*

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class BranchingCombinator extends Branching {

  private var branchings = Vector[Branching]()
  
  override def initialize(): Unit = {
    branchings.foreach(_.initialize())
  }

  override def alternatives(): Seq[Alternative] = {
	  var n = branchings.size
      var i = 0
      while (i < n) {
        val childs = branchings(i).alternatives()
        if (childs.nonEmpty) return childs
        i += 1
      }
      noAlternative
  }

  def addBranching(b: Branching) {
    branchings = branchings :+ b
  }

}
*/
