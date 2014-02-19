
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


abstract class abstractPermaFilter[T]{
  def notifyInsert(s: PFDLLStorageElement[T])
  def notifyDelete(s: PFDLLStorageElement[T])
}

class permaFilter[T,F](mFilter:T => Boolean, mMap:T=>F, filtered:PermaFilteredDoublyLinkedList[F]) extends abstractPermaFilter[T]{
  override def notifyInsert(s: PFDLLStorageElement[T]){
    if (mFilter(s.elem)){
      s.filtered = filtered.addElem(mMap(s.elem))
    }
  }
  def notifyDelete(s: PFDLLStorageElement[T]){
    filtered.deleteElem(s.filtered.asInstanceOf[PFDLLStorageElement[F]])
  }
}

/**
 *
 * @param mFilter this function is called on insert. It takes
 * -the inserted element,
 * -an insert function that performs the insert,
 * -a query function that can be called to check if the element is still in the list
 * @param mMap
 * @tparam T
 */
class delayedPermaFilter[T, F](mFilter:(T,=>(), => Boolean) => (),  mMap:T => F, filtered:PermaFilteredDoublyLinkedList[F]) extends abstractPermaFilter[T]{
  override def notifyInsert(inserted: PFDLLStorageElement[T]): Unit = {
    def insertIntoFiltered(){
      inserted.filtered = filtered.addElem(mMap(inserted.elem))
    }
    def isStillInList:Boolean = inserted.prev != null
    mFilter(inserted.elem,insertIntoFiltered(), isStillInList)
  }
  def notifyDelete(s: PFDLLStorageElement[T]){
    filtered.deleteElem(s.filtered.asInstanceOf[PFDLLStorageElement[F]])
  }
}

/**this is a mutable data structure that is able to represent sets through doubly-lined lists, with insert and delete in O(1) through reference
  * and to update in parallell another set that is a filter of the first one through a specified function
  * the filter can be specified anytime and filtering can be cascated, but a PermaFilteresDLL can have only one filter
  *
  * You should not perform any operation on the slave DLL,
  * although this will not be detected and reported as an error
  *
  * Beware that this is a mutable data structure, hence you should not perform any update on it while iterating on it.
  * @author renaud.delandtsheer@cetic.be
  * */
class PermaFilteredDoublyLinkedList[T <: AnyRef] extends Iterable[T]{

  private val headfantom:PFDLLStorageElement[T] = new PFDLLStorageElement[T](null.asInstanceOf[T])
  private val endfantom:PFDLLStorageElement[T] = new PFDLLStorageElement[T](null.asInstanceOf[T])
  headfantom.setNext(endfantom)


  /** this function is called on insert. It takes
    * -the inserted element,
    * -an insert function that performs the insert,
    * -a query function that can be called to check if the element is still in the list
    */
  private var permaFilter:abstractPermaFilter[T] = null

  /**returns the size of the PermaFilteredDLL*/
  override def size = msize

  private var msize:Int = 0

  /**adds an a item in the PermaFilteredDLL, and if accepted by the filter, adds it in the slave PermaFilteredDLL.
    * returns a reference that should be used to remove the item from all those structures at once.
    */
  def addElem(elem:T):PFDLLStorageElement[T] = {
    val d = new PFDLLStorageElement[T](elem)
    d.setNext(headfantom.next)
    headfantom.setNext(d)
    msize +=1

    permaFilter.notifyInsert(d)

    d
  }

  /**adds an element to the data structure, cfr. method addElem*/
  def +(elem:T){addElem(elem)}

  /**adds a bunch of items to the data structures*/
  def ++(elems:Iterable[T]) {for(elem <- elems) addElem(elem)}

  /**deletes an item from the DLL and all the filtered DLL.
    * the item is specified through the reference given when it was inserted in the first place.
    */
  def deleteElem(elemkey:PFDLLStorageElement[T]):T = {
    elemkey.prev.setNext(elemkey.next)
    elemkey.prev = null
    msize -=1

    if (permaFilter != null) permaFilter.notifyDelete(elemkey)

    elemkey.elem
  }

  override def isEmpty:Boolean = size == 0

  override def iterator = new PFDLLIterator[T](headfantom,endfantom)

  def delayedPermaFilter[F <: AnyRef](filter:(T,=>(), => Boolean) => (), mMap:T => F = _.asInstanceOf[F]):PermaFilteredDoublyLinkedList[F] = {
    assert(permaFilter == null,"DelayedPermaFilteredDoublyLinkedList can only accept a single filter")

    val filtered = new PermaFilteredDoublyLinkedList[F]

    permaFilter = new delayedPermaFilter[T, F](filter, mMap, filtered)

    var currentstorageElement:PFDLLStorageElement[T]=headfantom.next
    while(currentstorageElement!=endfantom){
      permaFilter.notifyInsert(currentstorageElement)
      currentstorageElement = currentstorageElement.next
    }
    filtered
  }

  def permaFilter[F <: AnyRef](filter:T => Boolean, mMap:T => F = _.asInstanceOf[F]):PermaFilteredDoublyLinkedList[F] = {
    assert(permaFilter == null,"PermaFilteredDoublyLinkedList can only accept a single filter")

    val filtered = new PermaFilteredDoublyLinkedList[F]

    permaFilter = new permaFilter[T,F](filter,mMap,filtered)

    var currentstorageElement:PFDLLStorageElement[T]=headfantom.next
    while(currentstorageElement!=endfantom){
      permaFilter.notifyInsert(currentstorageElement)
      currentstorageElement = currentstorageElement.next
    }
    filtered
  }

  /**
   * @param fn the function to execute on each items included in this list
   * @tparam U the output type of the function
   * @return a list containing the result of executing fn on each element of the DLL. the list is in reverse order.
   */
  def mapToList[U](fn:T => U):List[U] = {
    val it = iterator
    var toReturn:List[U] = List.empty
    while(it.hasNext){
      toReturn = fn(it.next()) :: toReturn
    }
    toReturn
  }
}