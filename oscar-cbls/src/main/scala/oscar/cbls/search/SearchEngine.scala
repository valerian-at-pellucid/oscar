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

package oscar.cbls.search
import scala.util.Random

/**Provides a set of selectors to ease the development of search engines
 * @param isRandomized can be set to false if one wants a reproductible behavior of the search engine
 * @author renaud.delandtsheer@cetic.be
  * */
class SearchEngine(isRandomized: Boolean = true) extends SearchEngineTrait{
  setRandomized(isRandomized)
}

trait SearchEngineTrait{
  var RandomGenerator: Random = null

  var Randomized:Boolean = true
  setRandomized(true)
  def setRandomized(Randomized:Boolean){
    this.Randomized = Randomized

    RandomGenerator = (if (Randomized) {
      new Random()
    } else {
      new Random(0)
    })
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and maximizing f(r,s) among the allowed couples
   * this selector is not randomized; in case of tie breaks the first one is returned
   * @param st is optional and set to true if not specified
    */
  def selectMaxNR2[R,S](r: Iterable[R] , s: Iterable[S], f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s.toList) yield (rr,ss)
    selectMaxNR[(R,S)](flattened , (rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and maximizing f(r) among the allowed couples
   * this selector is not randomized; in case of tie breaks the first one is returned
   * @param st is optional and set to true if not specified
   */
  def selectMaxNR[R](r: Iterable[R] , f: R => Int, st: (R => Boolean) = ((r:R) => true)): R = {
    var GotOne: Boolean = false
    var MaxSoFar = 0
    var Val:R = null.asInstanceOf[R]
    for (i <- r) {
      if (st(i)) {
        val v = f(i)
        if (v > MaxSoFar || !GotOne) {
          GotOne = true
          Val = i
          MaxSoFar = v
        }
      }
    }
    if(GotOne)Val else null.asInstanceOf[R]
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and minimizing f(r,s) among the allowed couples
   * this selector is not randomized; in case of tie breaks the first one is returned
   * @param st is optional and set to true if not specified
    */
  def selectMinNR2[R,S](r: Iterable[R] , s: Iterable[S], f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    selectMaxNR2[R,S](r , s, -f(_,_), st)
  }

  /**return an element r that is allowed: st(r) is true, and minimizing f(r) among the allowed couples
   * this selector is not randomized; in case of tie breaks the first one is returned
   * @param st is optional and set to true if not specified
   */
  def selectMinNR[R](r: Iterable[R] , f: R => Int, st: (R => Boolean) = ((r:R) => true)): R = {
    selectMaxNR[R](r , -f(_), st)
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and maximizing f(r,s) among the allowed couples
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param st is optional and set to true if not specified
    */
  def selectMax2[R,S](r: Iterable[R] , s: Iterable[S], f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s.toList) yield (rr,ss)
    selectMax[(R,S)](flattened , (rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and maximizing f(r) among the allowed couples
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param st is optional and set to true if not specified
   */
  def selectMax[R](r: Iterable[R] , f: R => Int, st: (R => Boolean) = ((r:R) => true)): R = {
    var MaxSoFar = Int.MinValue
    var Val:List[R] = List.empty
    for (i <- r) {
      if (st(i)) {
        val v:Int = f(i)
        if (v > MaxSoFar || Val.isEmpty) {
          Val = List(i)
          MaxSoFar = v
        } else if (!Val.isEmpty && v == MaxSoFar) {
          Val = i :: Val
        }
      }
    }
    if(!Val.isEmpty){
      Val.apply(RandomGenerator.nextInt(Val.length))
    } else null.asInstanceOf[R]
  }

  /**return a couple (r,s) that is allowed: filter(r,s) is true, and minimizing f(r,s) among the allowed couples
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param s is optional and set to true if not specified
   */  
  def selectMin[R,S](r: Iterable[R] , s: Iterable[S]) (f: (R,S) => Int, filter: ((R,S) => Boolean)): (R,S) = {
     val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s.toList) yield (rr,ss)
    selectMin[(R,S)](flattened)((rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => filter(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and minimizing f(r) among the allowed couples
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param st is optional and set to true if not specified
   */
  def selectMin[R](r: Iterable[R])(f: R => Int, st: (R => Boolean) = ((r:R) => true)): R = {
    var MinSoFar = Int.MaxValue
    var Val:List[R] = List.empty
    for (i <- r) {
      if (st(i)) {
        val v = f(i)
        if (v < MinSoFar || Val.isEmpty) {
          Val = List(i)
          MinSoFar = v
        } else if (!Val.isEmpty && v == MinSoFar) {
          Val = i :: Val
        }
      }
    }
  
    if(!Val.isEmpty){
      Val.apply(RandomGenerator.nextInt(Val.length))
    } else null.asInstanceOf[R]
  }

  /**return a couple (r,s) of elements r that is allowed: st(r,s) is true
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param st is optional and set to true if not specified
   */
  def selectFrom2[R,S](r: Iterable[R] , s: Iterable[S], st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
      val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s.toList) yield (rr,ss)
      selectFrom[(R,S)](flattened , (rands:(R,S)) => st(rands._1,rands._2))
    }

  /**return an element r that is allowed: st(r) is true
   * this selector is randomized; in case of tie breaks the returned one is chosen randomly
   * @param st is optional and set to true if not specified
   */
  def selectFrom[R](r: Iterable[R], st: (R => Boolean) = null): R = {
    if (st == null){
      var i = RandomGenerator.nextInt(r.size) -1
      val it = r.iterator
      //TODO: is it possible to do to the nth directly instead of scrolling the iterator?
      while(i > 0){it.next(); i-=1}
      it.next()
    }else{
      val emptyRlist:List[R]=List.empty
      var Val:List[R] = r.foldLeft(emptyRlist)((acc,rr) => (if (st(rr)) (rr :: acc) else acc))
      if (Val.isEmpty) return null.asInstanceOf[R]
      var i = RandomGenerator.nextInt(Val.size)
      Val(i)
    }
  }

  /**return an element of the range. IT is selected randomly with a uniform distribution
   * this is performed in O(1)
   */
  def selectFromRange(r: Range): Int = {
    var i = RandomGenerator.nextInt(r.size)
    r.apply(i)
  }

  
  /**return the first couple of elements (r,s) that is allowed: st(r) is true
   *the order is lexicographic
   * @param st is optional and set to true if not specified
   */
  def selectFirst2[R,S](r: Iterable[R] , s: Iterable[S], st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
        val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s.toList) yield (rr,ss)
        selectFirst[(R,S)](flattened , (rands:(R,S)) => st(rands._1,rands._2))
      }

  /**return the first element r that is allowed: st(r) is true
   * @param st is optional and set to true if not specified
   */
  def selectFirst[R](r: Iterable[R],st: (R => Boolean) = ((r:R) => true)): R = {
      for(rr <- r) if(st(rr)) return rr
      null.asInstanceOf[R]
  }

  /**return the first element r that is allowed: st(r) is true
    * @param st is optional and set to true if not specified
    */
  def selectFirstDo[R](r: Iterable[R],st: (R => Boolean) = ((r:R) => true))(doIt:R => Unit, ifNone: (()=> Unit) = ()=>{println("no suitable item found")}): Unit = {
    for(rr <- r) if(st(rr)) {
      doIt(rr)
      return
    }
    ifNone()
  }

  /**returns a randomly chosen boolean (50%-50%)*/
  def flip(PercentTrue:Int = 50):Boolean = (RandomGenerator.nextInt(100) < PercentTrue)

  /**returns a random permutation of the integers in [0; N]*/
  def getRandomPermutation(N:Int):Iterator[Int] = {
    val intarray:Array[Int] = new Array(N)
    for(i <- 0 to N-1) intarray(i)=i
    for(i <- 0 to N-1){
      val other = selectFromRange(0 to N-1)
      val old = intarray(i)
      intarray(i) = intarray(other)
      intarray(other) = old
    }
    intarray.iterator
  }

  /**choses an laternative among a list of them, ponderated with their associated probability
   * the returned pair includes the probability, divided by the total priority, so if it is not one, things get corrected*/
  def PonderatedChoose[T](alternatives:Iterable[(T, Float)]):(T, Float) = {
    var Ptot:Float = alternatives.map(tAndP => tAndP._2).foldLeft[Float](0)((a:Float, b:Float) => a+b)
    var choice = Ptot*RandomGenerator.nextFloat()
    for (a <- alternatives){
      if (choice <= a._2){
        return (a._1,a._2/Ptot)
      }else{
        choice -= a._2
      }
    }
    null //should not happen
  }
}

/*
class selectFirst[R](r: Iterable[R],st: (R => Boolean)){
  def apply(doIt:R => Unit, ifNone: (()=> Unit)= ()=>{println("no suitable item found in " + r)}){
    for(rr <- r) if(st(rr)) {
      doIt(rr)
      return
    }
    ifNone()
  }

  def getIt:R ={
    for(rr <- r) if(st(rr)) return rr
    null.asInstanceOf[R]
  }
}

object selectFirst{
  def apply[R](r: Iterable[R],st: (R => Boolean) = ((r:R) => true)):selectFirst[R] = new selectFirst[R](r,st)
  implicit def selectToR[R](s:selectFirst[R]):R = s.getIt
}
*/