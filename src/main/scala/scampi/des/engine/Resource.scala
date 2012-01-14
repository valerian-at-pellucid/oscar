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


/**
 * Capacitated resource where waiting customers are served in FIFO order
 * @author pschaus
 */
class Resource(m : Model, capacity: Int) {
	
	private var n = 0
	private var pendings = new DoubleLinkedList ()
	
	def request(block: => Unit) {
		if (n < capacity) {
	         n += 1
	         block
	    } else {
	         pendings :+ block;
	    }
	}
	
	def release() {
		n -= 1
		if (pendings.nonEmpty) {
			val block = pendings.head
			pendings = pendings.drop(1)
			block
		}
	}
	
}

class UnaryResource(m : Model) extends Resource(m,1)