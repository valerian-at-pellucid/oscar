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


package oscar.cbls.modeling

import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.lib.numeric._
import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.lib.set._

trait Invariants extends ClusterInvariants
with ComplexLogicInvariants
with ElementInvariants
with MinMaxInvariants
with NumericInvariants
with SetInvariants

/**
 * modeling interface presenting the cluster invariants
 * @author renaud.delandtsheer@cetic.be
*/
trait ClusterInvariants{

  def makeSparseCluster(values:Array[CBLSIntVar], clusters: Iterable[Int]):SparseCluster = Cluster.MakeSparse(values, clusters)

  def makeDenseCluster(values:Array[CBLSIntVar]):DenseCluster = Cluster.MakeDense(values)

  def makeDenseClusterAssumingMinMax(values:Array[CBLSIntVar],themin:Int,themax:Int):DenseCluster = Cluster.MakeDenseAssumingMinMax(values,themin,themax)

  /**maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
   * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
   * */
  def sparseCluster(values:Array[CBLSIntVar], Clusters:SortedMap[Int,CBLSSetVar]) = SparseCluster(values:Array[CBLSIntVar], Clusters:SortedMap[Int,CBLSSetVar])

  /**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
   * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
   * */
  def denseCluster(values:Array[CBLSIntVar], clusters:Array[CBLSSetVar]) = DenseCluster(values:Array[CBLSIntVar], clusters:Array[CBLSSetVar])

  /**Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i] == j}
   * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
   * */
  def denseCount(values:Array[CBLSIntVar], counts:Array[CBLSIntVar]) = DenseCount(values:Array[CBLSIntVar], counts:Array[CBLSIntVar])

  /**maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
   * */
  def denseRef(references:Array[CBLSSetVar], referencing:Array[CBLSSetVar]) = DenseRef(references:Array[CBLSSetVar], referencing:Array[CBLSSetVar])

  /**
   * Maintains a resource usage profile.
   * @param indices the indices of tasks
   * @param start the start time of tasks
   * @param duration the duration of tasks
   * @param amount the amount that tasks use of this resource
   * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
   * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
   */
  def cumulative(indices:Array[Int], start:Array[CBLSIntVar], duration:Array[CBLSIntVar], amount:Array[CBLSIntVar], profile:Array[CBLSIntVar], active:Array[CBLSSetVar])  =
    Cumulative(indices:Array[Int], start:Array[CBLSIntVar], duration:Array[CBLSIntVar], amount:Array[CBLSIntVar], profile:Array[CBLSIntVar], active:Array[CBLSSetVar])


  /** { i in index(values) | cond(values[i] }
   * @param values is an array of IntVar
   * @param cond is a function that selects values to be includes in the output set.
   * This ''cond'' function cannot depend on any IntVar, as updates to these IntVars will not trigger propagation of this invariant.
   */
  def filter(values:Array[CBLSIntVar], cond:(Int=>Boolean)) = Filter(values:Array[CBLSIntVar], cond:(Int=>Boolean))

  /** {i in index of values | values[i] <= boundary}
   * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
   * @param values an array of intvar
   * @param boundary the boundary for comparison
   */
  def selectLEHeapHeap(values:Array[CBLSIntVar], boundary: CBLSIntVar) = SelectLEHeapHeap(values:Array[CBLSIntVar], boundary: CBLSIntVar)


  /**{i \in index of values | values[i] <= boundary}
   * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
 - SelectLESetQueue does not allow boundary to decrease
 - SelectLESetQueue does not allow elements above boundary to change
 - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
   * @param values: an array of intvar
   * @param boundary: the boundary for comparison
   */
  def selectLESetQueue(values:Array[CBLSIntVar], boundary: CBLSIntVar) = SelectLESetQueue(values:Array[CBLSIntVar], boundary: CBLSIntVar)

}

/**
 * modeling interface presenting the complex logic invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ComplexLogicInvariants{

  /**this invariants maintains data structures representing vrp of vehicles.
   * for use in TSP, VRP, etc.
   * arrays start at one until N
   * position 0 is to denote an unrouted node.
   * The nodes from 1 to V are the starting points of vehicles.
   *
   * @param V the number of vrp to consider V>=1 and V<=N
   */
  def routes(V: Int, Next:Array[CBLSIntVar]) = Routes.buildRoutes(Next, V)

  /**maintains a sorting of the ''values'' array:
   * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
   * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
   * */
  def sort(values:Array[CBLSIntVar], ReversePerm:Array[CBLSIntVar]) = Sort(values:Array[CBLSIntVar], ReversePerm:Array[CBLSIntVar])

  /**returns the ForwardPerm for a given array
   * It instantiates an array of the appropriate size and populates it with IntVar.
   */
  def makeSort(values:Array[CBLSIntVar]):Sort = Sort.MakeSort(values:Array[CBLSIntVar])
}


/**
 * modeling interface presenting the element invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ElementInvariants{
  /** if (ifVar >0) then thenVar else elveVar
   * @param ifVar the condition (IntVar)
   * @param thenVar the returned value if ifVar > 0
   * @param elseVar the returned value if ifVar <= 0
   * */
  def intITE(ifVar:CBLSIntVar, thenVar:CBLSIntVar, elseVar:CBLSIntVar) = IntITE(ifVar, thenVar, elseVar)

  /** inputarray[index]
   * @param inputarray is an array of IntVar
   * @param index is the index accessing the array*/
  def intElement(index:CBLSIntVar, inputarray:Array[CBLSIntVar]) = IntElement(index:CBLSIntVar, inputarray:Array[CBLSIntVar])

  /**Union(i in index) (array[i])
   * @param index is an IntSetVar denoting the set of positions in the array to consider
   * @param inputarray is the array of intvar that can be selected by the index
   */
  def intElements(index:CBLSSetVar, inputarray:Array[CBLSIntVar]) = Elements(index, inputarray)

  /** inputarray[index] on an array of IntSetVar
   * @param inputarray is the array of intsetvar
   * @param index is the index of the array access
   **/
  def intSetElement(index:CBLSIntVar, inputarray:Array[CBLSSetVar]) = SetElement(index, inputarray)
}

/**
 * modeling interface presenting the min-max invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait MinMaxInvariants{

  /** Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
   * @param vars is an array of IntVar, which can be bulked
   * @param cond is the condition, supposed fully acceptant if not specified
   * @param default is the value returned when cond is empty
   * update is O(log(n))
   * */
  def argMax(vars: Array[CBLSIntVar], cond: CBLSSetVar = null,default:Int = Int.MinValue) = ArgMaxArray(vars, cond,default)


  /** Maintains {i in indices of (varss Inter cond) | varss[i] == min(varss(i in indices of (varss Inter cond))}
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default is the value returned when cond is empty
   * update is O(log(n))
   * */
  def argMin(varss: Array[CBLSIntVar], ccond: CBLSSetVar = null, default:Int = Int.MaxValue) = ArgMinArray(varss, ccond, default)

  /** maintains output = Max(a,b)
   * where output, a, and b are an IntVar
   * use this if you only have two variables to max, otherwise, refer to log iplementations
   * */
  def max2(a: CBLSIntVar, b: CBLSIntVar) = Max2(a, b)

  /** maintains output = Min(a,b)
   * where output, a, and b are an IntVar
   * use this if you only have two variables to max, otherwise, refer to log iplementations
   * */
  def min2(a: CBLSIntVar, b: CBLSIntVar) = Min2(a: CBLSIntVar, b: CBLSIntVar)

  /** Maintains Max(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * */
  def maxArray(varss: Array[CBLSIntVar], ccond: CBLSSetVar = null, default: Int = Int.MinValue) = MaxArray(varss, ccond, default)

  /** Maintains Min(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * */
  def minArray(varss: Array[CBLSIntVar], ccond: CBLSSetVar = null, default: Int = Int.MaxValue) = MinArray(varss, ccond, default)

  /** maintains output = Min(v)
   * where
   * * output is an IntVar
   * * v is an IntSetVar
   * @param default is the default value if v is empty
   * update is O(log(n))
   * */
  def minSet(v: CBLSSetVar, default: Int = Int.MaxValue) = MinSet(v, default)

  /** maintains output = Max(v)
   * where
   * * output is an IntVar
   * * v is an IntSetVar
   * @param default is the default value if v is empty
   * update is O(log(n))
   * */
  def maxSet(v: CBLSSetVar, default: Int = Int.MinValue) = new MaxSet(v, default)
}

/**
 * modeling interface presenting the numeric invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait NumericInvariants{
  /** sum(vars)
   * @param vars is an iterable of IntVars
   * */
  def sum(vars:Iterable[CBLSIntVar]) = Sum(vars:Iterable[CBLSIntVar])

  /** prod(vars)
   * @param vars is a set of IntVars
   * */
  def prod(vars:Iterable[CBLSIntVar]) = Prod(vars:Iterable[CBLSIntVar])

  /** left - right
   * where left, right, and output are IntVar*/
  def minus(left:CBLSIntVar, right:CBLSIntVar) = Minus(left:CBLSIntVar, right:CBLSIntVar)

  /** left + right
   * where left, right, and output are IntVar*/
  def sum2(left:CBLSIntVar, right:CBLSIntVar) = Sum2(left:CBLSIntVar, right:CBLSIntVar)

  /** left * right
   * where left, right, and output are IntVar*/
  def prod2(left:CBLSIntVar, right:CBLSIntVar) = Prod2(left:CBLSIntVar, right:CBLSIntVar)

  /**left / right
   * where left, right, and output are IntVar
   * do not set right to zero, as usual... */
  def div(left:CBLSIntVar, right:CBLSIntVar) = Div(left:CBLSIntVar, right:CBLSIntVar)

  /**left / right
   * where left, right, and output are IntVar
   * do not set right to zero, as usual... */
  def mod(left:CBLSIntVar, right:CBLSIntVar) = Mod(left:CBLSIntVar, right:CBLSIntVar)

  /**abs(v) (absolute value)
   * where output and v are IntVar*/
  def abs(v:CBLSIntVar) = Abs(v:CBLSIntVar)

  /**Maintains output to the smallest value such that
    * output >= from
    * (output - shift) MOD period > zone
    * (output - shift + length) MOD period > zone
    * of course, it is required that length is < period - zone, and exception is thrown otherwise.
    *
    * For instance, suppose that some task can only happen during open day (Mon-Fri),
    * let 'from" being the lowest starting date, and 'length' its duration.
    * the invariant will check that the task can be finished by friday of the week, and if not,
    * will propose the next monday. 'shift' specifies says what is the starting day at zero.
    * zone is the forbidden zone. it starts at the beginning of the cycle.
    *
    * for instance, suppose you represent days starting from zero, and zero is a monday,
    * and you want to round up to the next open day (sa and su are closed day, the correct declaration is:
    * RoundUpModulo(from,duration,7,2,5)
    *
    * @param from the starting date of the task. it can start later.
    * @param duration the duration of the task.
    * @param period the period of the forbidden-allowed pattern
    * @param zone the size of the forbidden zone. it starts at the beginning of the period
    * @param shift the first period starts later than zero. it starts at shift. the duration before its start is allowed.
    */

    def roundUpModulo(from: CBLSIntVar, duration: CBLSIntVar, period: Int, zone: Int, shift: Int) = RoundUpModulo(from: CBLSIntVar, duration: CBLSIntVar, period: Int, zone: Int, shift: Int)

  /**Maintains output to the smallest value such that
    * output >= from
    * the interval [output ; output + length] does not overlap with the intervals given in FobiddenZones
    *
    * @param from
    * @param duration
    * @param ForbiddenZones
    */
  def roundUpCustom(from: CBLSIntVar, duration: CBLSIntVar, ForbiddenZones: List[(Int, Int)]) = RoundUpCustom(from: CBLSIntVar, duration: CBLSIntVar, ForbiddenZones: List[(Int, Int)])

  /**
   * This invariant implements a step function. Values higher than pivot are mapped to ifval
   * values lower or equal to pivot are mapped to elseval
   * This invariant was suggested by Jean-Noël Monette
   *
   * @param x the IntVar parameter of the invariant
   * @param pivot the pivot value
   * @param thenval the value returned when x > pivot
   * @param elseval the value returned when x <= pivot
   */
  def step(x:CBLSIntVar,pivot:Int = 0,thenval:Int = 1,elseval:Int = 0) = Step(x:CBLSIntVar,pivot:Int,thenval:Int ,elseval:Int)

  /** sum(i in cond) vars(i)
   * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
   * @param vars is a set of IntVars
   * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
   */
  def sumElements(vars: Array[CBLSIntVar], cond: CBLSSetVar) = SumElements(vars: Array[CBLSIntVar], cond: CBLSSetVar)

  /** prod(i in cond) vars(i)
   * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
   * @param vars is a set of IntVars
   * @param cond is the condition for selecting variables in the set of summed ones.
   */
  def prodElements(vars: Array[CBLSIntVar], cond: CBLSSetVar) = ProdElements(vars: Array[CBLSIntVar], cond: CBLSSetVar)

}

/**
 * modeling interface presenting the set invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait SetInvariants{
  /** left UNION right
   * @param left is an intvarset
   * @param right is an intvarset
   * */
  def union(left:CBLSSetVar, right:CBLSSetVar) = Union(left:CBLSSetVar, right:CBLSSetVar)

  /** left INTER right
   * @param left is an intvarset
   * @param right is an intvarset
   * */
  def inter(left:CBLSSetVar, right:CBLSSetVar) = Inter(left:CBLSSetVar, right:CBLSSetVar)

  /** left MINUS right, the set diff operator
   * @param left is the base set
   * @param right is the set that is removed from left
   * */
  def diff(left:CBLSSetVar, right:CBLSSetVar) = Diff(left:CBLSSetVar, right:CBLSSetVar)

  /** #(v) (cardinality)
   * @param v is an IntSetVar, the set of integers to count
   */
  def cardinality(v:CBLSSetVar) = Cardinality(v:CBLSSetVar)

  /** makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
   * @param on is a set of IntVar
   * */
  def makeSet(on:SortedSet[CBLSIntVar]) = MakeSet(on:SortedSet[CBLSIntVar])

  /** makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
   * @param lb is the lower bound of the interval
   * @param ub is the upper bound of the interval
   * */
  def interval(lb:CBLSIntVar,ub:CBLSIntVar) = Interval(lb:CBLSIntVar,ub:CBLSIntVar)

  /**maintains the output as any value taken from the intset var parameter.
   * if this set is empty, puts the default value ni output.
   * @param from
   * @param default
   */
  def takeAny(from:CBLSSetVar,  default:Int) = TakeAny(from:CBLSSetVar,  default:Int)

  /** Sum(i in on)(fun(i))
   * @param on is the set of integers to add
   * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
   * */
  def setSum(on:CBLSSetVar, fun:(Int => Int) = ((a:Int) => a)) = SetSum(on, fun)

  /** PRod(i in on)(fun(i))
   * @param on is the set of integers to multiply
   * @param fun is an optional function Int -> Int to apply before multiplying elements. It is expected not to rely on any variable of the model.
   * */
  def setProd(on:CBLSSetVar, fun:(Int => Int) = ((a:Int) => a)) = SetProd(on, fun)
}
