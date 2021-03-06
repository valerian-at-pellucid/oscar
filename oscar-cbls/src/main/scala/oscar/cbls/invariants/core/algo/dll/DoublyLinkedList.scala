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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.core.algo.dll

/** This is a mutable data structure allowing insert,
  * and delete in O(1) based on a key mechanism
  * @author renaud.delandtsheer@cetic.be
  * @tparam T
  */
class DoublyLinkedList[T] extends Iterable[T]{

  val headfantom:DLLStorageElement[T] = new DLLStorageElement[T](null.asInstanceOf[T])
  val endfantom:DLLStorageElement[T] = new DLLStorageElement[T](null.asInstanceOf[T])

  headfantom.setNext(endfantom)

  override def size = msize
  var msize:Int = 0
  def addElem(elem:T):DLLStorageElement[T] = {
    val d = new DLLStorageElement[T](elem)
    d.setNext(headfantom.next)
    headfantom.setNext(d)
    msize +=1
    d
  }

  def +(elem:T){addElem(elem)}
  def ++(elems:Iterable[T]) {for(elem <- elems) addElem(elem)}

  def deleteElem(elemkey:DLLStorageElement[T]):T = {
    elemkey.prev.setNext(elemkey.next)
    msize -=1
    elemkey.elem
  }

  def dropAll(){
    headfantom.setNext(endfantom)
    msize = 0
  }

  override def isEmpty:Boolean = (size == 0)

  override def iterator = new DLLIterator[T](headfantom,endfantom)
}

/**
* @author renaud.delandtsheer@cetic.be
 * @param elem
 * @tparam T
 */
class DLLStorageElement[T](val elem:T){
  var next:DLLStorageElement[T] = null
  var prev:DLLStorageElement[T] = null

  def setNext(d:DLLStorageElement[T]){
    this.next = d
    d.prev = this
  }
}

/**
   * @author renaud.delandtsheer@cetic.be
 * @param CurrentKey
 * @param endfantom
 * @tparam T
 */
class DLLIterator[T](var CurrentKey:DLLStorageElement[T], val endfantom:DLLStorageElement[T]) extends Iterator[T]{
  def next():T = {
    CurrentKey = CurrentKey.next
    CurrentKey.elem
  }

  def hasNext:Boolean = {CurrentKey.next != endfantom}
}
