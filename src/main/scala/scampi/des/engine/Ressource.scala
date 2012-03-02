/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.des.engine


import scala.collection.mutable._
import scala.util.continuations._
import scampi.invariants._

/**
 * Capacitated resource where waiting customers are served in FIFO order
 * @author Pierre Schaus, Sebastien Mouthuy
 */

class Resource(m : Model, capacity: Int) {
	
	private val n = new VarInt(m, 0)
	
	def pendings = n
	def request(): Unit @suspendable = {
	  waitFor( n.filter( _ < capacity ))	 	  
	  n :+= 1
	}
	  
	 	
	def release() {
	  n :-= 1
	}
	def isEmpty = {
    n() == 0
	}
}

class UnaryResource(m : Model) extends Resource(m,1)