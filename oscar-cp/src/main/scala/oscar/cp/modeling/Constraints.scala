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


package oscar.cp.modeling

import java.util.LinkedList
import scala.Vector
import scala.collection.IndexedSeq
import scala.collection.Iterable
import scala.collection.immutable.Set
import oscar.cp.constraints.AllDifferent
import oscar.cp.constraints.Among
import oscar.cp.constraints.AtLeastNValue
import oscar.cp.constraints.Automaton
import oscar.cp.constraints.BinPacking
import oscar.cp.constraints.BinPackingFlow
import oscar.cp.constraints.BinaryKnapsack
import oscar.cp.constraints.BinarySum
import oscar.cp.constraints.Circuit
import oscar.cp.constraints.Count
import oscar.cp.constraints.CountCst
import oscar.cp.constraints.CountSimple
import oscar.cp.constraints.Deviation
import oscar.cp.constraints.Disjoint
import oscar.cp.constraints.ElementCst
import oscar.cp.constraints.ElementCst2D
import oscar.cp.constraints.ElementVar
import oscar.cp.constraints.GCC
import oscar.cp.constraints.GCCVar
import oscar.cp.constraints.Inverse
import oscar.cp.constraints.Knapsack
import oscar.cp.constraints.LexLeq
import oscar.cp.constraints.Maximum
import oscar.cp.constraints.MaxCumulative
import oscar.cp.constraints.MinAssignment
import oscar.cp.constraints.Minimum
import oscar.cp.constraints.Modulo
import oscar.cp.constraints.Or
import oscar.cp.constraints.OrReif
import oscar.cp.constraints.Permutation
import oscar.cp.constraints.Regular
import oscar.cp.constraints.SoftGCC
import oscar.cp.constraints.Spread
import oscar.cp.constraints.Stretch
import oscar.cp.constraints.SweepMinCumulative
import oscar.cp.constraints.TableData
import oscar.cp.constraints.UnaryResource
import oscar.cp.constraints.WeightedSum
import oscar.cp.constraints.stockingCost.StockingCost
import oscar.cp.core.CPBoolVar
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPSetVar
import oscar.cp.core.Constraint
import oscar.cp.core._
import oscar.cp.constraints.TableSTR2

trait Constraints {

  /**
   * Bin-Packing Constraint linking the placement variables of sized items into bins with
   * the total size of the bins
   * @param x with x(i) is the bin where the item i is placed
   * @param w with w(i) is the size of item i
   * @param l with l(j) is the load of bin j
   * @return a binpacking constraint linking the variables in argument such that l[i] == sum,,j,, w[j]*(x[j]==i) for all bins i
   */
  def binPacking(x: IndexedSeq[CPIntVar], w: IndexedSeq[Int], l: IndexedSeq[CPIntVar]): Constraint = {
    return new BinPacking(x.toArray, w.toArray, l.toArray)
  }

  @deprecated("use binPacking instead", "1.0")
  def binpacking(x: IndexedSeq[CPIntVar], w: IndexedSeq[Int], l: IndexedSeq[CPIntVar]) = binPacking(x, w, l)

  /**
   * Bin-Packing Constraint linking the placement variables of sized items into bins with
   * the total size of the bins
   * @param x with x(i) is the bin where the item i is placed
   * @param w with w(i) is the size of item i
   * @param l with l(j) is the load of bin j
   * @param c with c(j) is the cardinality of bin j (number of items)
   * @return a binpacking constraint linking the variables in argument such that l[i] == sum,,j,, w[j]*(x[j]==i) for all bins i and
   */
  def binPackingCardinality(x: IndexedSeq[CPIntVar], w: IndexedSeq[Int], l: IndexedSeq[CPIntVar], c: IndexedSeq[CPIntVar]): Constraint = {
    return new BinPackingFlow(x.toArray, w.toArray, l.toArray, c.toArray)
  }

  /**
   * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param w with w(i) is the weight of item i
   * @param l the load of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], w: IndexedSeq[Int], W: CPIntVar): Constraint = {
    return new BinaryKnapsack(x.toArray, w.toArray, W)
  }

  /**
   * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param w with w(i) is the weight of item i
   * @param l the load of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], w: IndexedSeq[Int], W: Int): Constraint = {
    return new BinaryKnapsack(x.toArray, w.toArray, CPIntVar(W)(x(0).store))
  }

  /**
   * Binary-Knapsack Constraint computing the total profit of items placed into a capacitated knapsack
   * @param x with x(i) == 1 if the item is selected, 0 otherwise
   * @param p with p(i) >= 0 is the profit you get selecting item i
   * @param w with w(i) > 0 is the weight of item i into the knapsack
   * @param P the total profit of the knapsack
   * @param W the total weight of the knapsack
   * @return a binary-knapsack constraint linking the variables in argument such that P == sum,,j,, p[j]*x[i] and W == sum,,j,, w[j]*x[i]
   */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], p: IndexedSeq[Int], w: IndexedSeq[Int], P: CPIntVar, W: CPIntVar): Knapsack = {
    return new Knapsack(x.toArray, p.toArray, w.toArray, P, W)
  }
  
  /**
   * atLeastNValue Constraint (Available Filtering: Weak, Strong)
   * @param vars an non empty array of variables
   * @return a constraint ensuring that nValue is the number of different valuesin vars
   */
  def atLeastNValue(vars: Iterable[CPIntVar], nValue: CPIntVar): Constraint = {
    return new AtLeastNValue(vars.toArray,nValue)
  }  

  /**
   * allDifferent Constraint (Available Filtering: Weak, Strong)
   * @param vars an non empty array of variables
   * @return a constraint ensure that no value occurs more than once in vars
   */
  def allDifferent(vars: CPIntVar*): Constraint = {
    return new AllDifferent(vars: _*)
  }

  def allDifferent(vars: Iterable[CPIntVar]): Constraint = {
    return allDifferent(vars.toArray: _*)
  }
  
  /**
   * minAssignment Constraint (Available Filtering: Medium)
   * @param vars an non empty array of variables, weights a n x n array (n = vars.size-1).
   * @return a constraint ensure that allDifferent(x) and sum(i) weights(i)(x(i)) <= cost 
   */  
  def minAssignment(x: Array[CPIntVar], weights: Array[Array[Int]], cost: CPIntVar): MinAssignment = {
    return new MinAssignment(x,weights,cost)
  } 

  /**
   * @param succ[i] is the successor of node i (also place i inside the domain of succ[i] if you want to allow it not to be part of the path
   * @param start start is the index of the first node on the path
   * @param end is the index of the last node on the path
   * @param length is the length of the path (number edges)
   *
   * Example:
   * succ [1, 3, 2, 5, 4, 0], start = 0, end = 5, length = 3 represents the path 0 -> 1 -> 3 -> 5
   * Notice that nodes that do not belong to the path, have them-self as successor and that
   * the successor of the last node of the path is the first node by convention
   * @author Pierre Schaus
   */

  //  def path(succ: Array[CPIntVar], start: CPIntVar, end: CPIntVar, length: CPIntVar): Constraint = {
  //    return new Path(succ,start,end,length)
  //  }

  /**
   * Circuit Constraint (Available Filtering: Weak, Strong)
   * @param vars an array of n variable with domains defined on (0..n-1)
   * @return a constraint enforcing a circuit representation where vars(i) represents
   *         the city visited after city i (no city is visited twice and there is no sub-tours).
   *
   */
  def circuit(vars: Array[CPIntVar]): Constraint = {
    return new Circuit(vars)
  }

  /**
   * Lexicographically Less or Equal Constraint
   * @param x an non empty array of variables x1, x2, x3 ... xn
   * @param y an array y1, y2, y3 ... yn (of the same size as x)
   * @return a constraint enforcing x LexLeq y i.e. :
   * 		 x1 < y1 or (x2,x3,...,xn) LexLeq (y2,y3,...,yn)
   */
  def lexLeq(x: Array[CPIntVar], y: Array[CPIntVar]): Constraint = {
    new LexLeq(x, y)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @return a variable z linked to tab and x by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPIntVar, strength: CPPropagStrength = CPPropagStrength.Medium): CPIntVar = {
    val minval = tab.min
    val maxval = tab.max
    val z = CPIntVar(minval, maxval)(x.store)
    x.store.post(new ElementCst(tab.toArray, x, z), strength)
    z
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPIntVar, z: CPIntVar): Constraint = {
    new ElementCst(tab.toArray, x, z)
  }

  /**
   * Element Constraint, indexing an array of integers by a variable
   * @param tab an non empty array n integers
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def element(tab: IndexedSeq[Int], x: CPIntVar, z: Int): Constraint = {
    new ElementCst(tab.toArray, x, CPIntVar(z, z)(x.store))
  }

  /**
   * Element 2D Constraint, indexing an integer matrix by two index variables
   * @param matrix rectangle matrix of sizes n x m
   * @param i the first index variable (line index) with domain defined on (0..n-1)
   * @param j the second index variable (column index) with domain defined on (0..m-1)
   * @return a variable z linked to the arguments with the relation matrix(i)(j) == z
   */
  def element(matrix: Array[Array[Int]], i: CPIntVar, j: CPIntVar): CPIntVar = {
    val z = CPIntVar(matrix.flatten.min to matrix.flatten.max)(i.store)
    val ok = i.store.post(new ElementCst2D(matrix, i, j, z))
    assert(ok != CPOutcome.Failure, { println("element on matrix, should not fail") })
    return z
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @return an integer variable z such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, l: CPPropagStrength = Weak): CPIntVar = {
    val minval = (for (x <- tab) yield x.getMin) min
    val maxval = (for (x <- tab) yield x.getMax) max
    val z = CPIntVar(minval, maxval)(x.store)
    x.store.add(new ElementVar(tab.map(_.asInstanceOf[CPIntVar]).toArray, x, z), l)
    z
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer variable
   * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: CPIntVar): Constraint = {
    new ElementVar(tab.map(_.asInstanceOf[CPIntVar]).toArray, x, z)
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: Int): Constraint = {
    new ElementVar(tab.toArray, x, CPIntVar(z)(x.store))
  }

  /**
   * Element Constraint, indexing an array of variables by a variable
   * @param tab an non empty array n variables
   * @param x an index variable with domain defined on (0..n-1)
   * @param z an integer
   * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
   */
  def elementVar(tab: IndexedSeq[CPBoolVar], x: CPIntVar, z: Boolean): Constraint = {
    val z_ = CPBoolVar(z)(x.store)
    new ElementVar(tab.map(_.asInstanceOf[CPIntVar]).toArray, x, z_)
  }
  
  /**
   * Inverse constraint
   * @param prev an array of `n` integer variables
   * @param next an array of `n` integer variables 
   * @return a constraint enforcing for all `i` in `0 until n`:
   *         1. `next(prev(i)) == i`
   *         2. `prev(next(i)) == i`
   */
  def inverse(prev: Array[CPIntVar], next: Array[CPIntVar]): Inverse = new Inverse(prev, next)

  /**
   * Sum Constraint
   * @param vars a non empty array of n variables
   * @param s a variable representing the sum of vars
   * @return a constraint enforcing vars(0)+vars(1)+...+vars(n) = s
   */
  def sum(vars: Array[CPIntVar], s: CPIntVar): Constraint = {
    /*
    var x = vars
    while (x.size > 2) {
      //System.err.println("sum"+x.size)
      val y = x.sliding(2, 2).toArray
      x = y.map{ arg =>
      	if (arg.size == 2) arg(0)+arg(1)
      	else arg(0)
      }.reverse
    }
    if (x.size == 2) new BinarySum(x(0),x(1),s)
    else x(0) == s
    */
    if (vars.size == 2) new BinarySum(vars(0),vars(1),s) 
    else new oscar.cp.constraints.Sum(vars, s)
  }

  /**
   * Sum Constraint
   * @param vars a non empty array of n variables
   * @return a variable representing vars(0)+vars(1)+...+vars(n)
   */
  def sum(vars: Iterable[CPIntVar]): CPIntVar = {
    if (vars.size == 1) return vars.head
    val x = vars.toArray
    val minVal = (0 /: vars) {
      (sum, v) => sum + v.getMin
    }
    val maxVal = (0 /: vars) {
      (sum, v) => sum + v.getMax
    }
    val s = CPIntVar(minVal, maxVal)(x(0).store)
    x(0).store.post(sum(x, s))
    s
  }

  /**
   * Sum Constraint
   * @param indexes
   * @param f a function mapping to a CPIntVar
   * @return a variable representing vars(f(i0))+vars(f(i1))+...+vars(f(in)) with i0, i1...,in the indexes
   */
  def sum[A](indexes: Iterable[A])(f: A => CPIntVar): CPIntVar = sum(indexes map f)

  /**
   * Sum Constraint
   * @param indexes1 a first iterable
   * @param indexes2 a second iterable
   * @param f a function mapping A,B to a variable with A from indexes1 and B from indexes2
   * @return a variable that is the sum of f(A,B) over each (A, B) in (indexes x indexes2)
   */
  def sum[A, B](indexes1: Iterable[A], indexes2: Iterable[B])(f: (A, B) => CPIntVar): CPIntVar = {
    sum(for (i <- indexes1; j <- indexes2) yield f(i, j))
  }

  /**
   * Sum Constraint
   * @param n1 size of the first range of i
   * @param n2 size of the second range of j
   * @param f a function mapping the indices i and j to a variable
   * @return a variable that is the sum of f(i, j) over each (i, j) in (0 until n1 x 0 until n2)
   */
  def sum(n1: Int, n2: Int)(f: (Int, Int) => CPIntVar): CPIntVar = {
    sum(0 until n1, 0 until n2)(f)
  }

  /**
   * Weighted Sum Constraint
   * @param indexes an iterable of index values
   * @param a function: i => (w_i, x_i) where i is an index from indexes
   * @return a variable S linked with the relation S = sum(i in indexes) (w_i * x_i)
   */
  def weightedSum[A](indexes: Iterable[A])(f: A => (Int, CPIntVar)): CPIntVar = {
    val (w,x) = (for (i <- indexes) yield f(i)).unzip
    weightedSum(w.toArray,x.toArray)
  }

  /**
   * Weighted Sum Constraint
   * @param indexes1 an iterable of index values
   * @param indexes2 an iterable of index values
   * @param a function: (i,j) => (w_ij, x_ij) where i is an index from indexes1, j is an index from indexes2
   * @return a variable S linked with the relation S = sum(i in indexes1,j in indexes2) (w_ij * x_ij)
   */
  def weightedSum[A, B](indexes1: Iterable[A], indexes2: Iterable[B])(f: (A, B) => (Int, CPIntVar)): CPIntVar = {
    val (w,x) = (for (i <- indexes1; j <- indexes2) yield f(i,j)).unzip
    weightedSum(w.toArray,x.toArray)
  }
  
  /**
   * Weighted Sum Constraint
   * @return y == sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPIntVar], y: CPIntVar): Constraint = {
    new WeightedSum(w,x,y)
  }
  
  /**
   * Weighted Sum Constraint
   * @return y==sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPIntVar], y: Int): Constraint = {
    weightedSum(w,x,CPIntVar(y)(x(0).store))
  }  

  /**
   * Weighted Sum Constraint
   * @return sum(i)(w_i * x_i)
   */
  def weightedSum(w: Array[Int], x: Array[CPIntVar]): CPIntVar = {
    val cp = x(0).store
    val m = w.zip(x).map{case(wi,xi) => if (wi < 0) wi*xi.max else wi*xi.min}.sum
    val M = w.zip(x).map{case(wi,xi) => if (wi < 0) wi*xi.min else wi*xi.max}.sum
    val y = CPIntVar(m to M)(cp)
    cp.post(weightedSum(w,x,y))
    y
  }

  /**
   * Weighted Sum Constraint
   * @return sum(ij)(w_ij * x_ij)
   */
  def weightedSum(w: Array[Array[Int]], x: Array[Array[CPIntVar]]): CPIntVar = {
    weightedSum(w.flatten,x.flatten)
  }

  /**
   * Or (logical) Constraint
   * @param vars a non empty array of n variables
   * @param z the result of the or over vars
   * @return an or constraint
   */  
  def or(vars: Iterable[CPBoolVar], z:CPBoolVar): Constraint = {
    if (z.isTrue) or(vars)
    else new OrReif(vars.toArray, z)
    //new OrReif2(vars, z)
  }
  
  /**
   * Or (logical) Constraint
   * @return a constraint such that at least one variables in vars must be true
   */
  def or(vars: Iterable[CPBoolVar]): Constraint = {
    new Or(vars.toArray)
    //sum(vars) >= 1
  }

  /**
   * Or (logical) Constraint
   * @return a constraint such that at least one variables in vars must be true
   */
  def or[A](indexes: Iterable[A])(f: A => CPBoolVar): Constraint = {
    or((for (i <- indexes) yield f(i)))
  }

  /**
   * Or (logical) Constraint
   * @param vars a non empty array of n variables
   * @return result of the or over vars
   */
  def isOr(vars: Iterable[CPBoolVar]): CPBoolVar = {
    val z = new CPBoolVar(vars.head.store)
    vars.head.store.add(or(vars, z))
    z
  }

  /**
   * Or (logical) Constraint
   * @return z the result of the or over or(f(i))
   */  
  def isOr[A](indexes: Iterable[A])(f: A => CPBoolVar): CPBoolVar = {
    val x = (for (i <- indexes) yield f(i)).toArray
    val z = new CPBoolVar(x(0).store)
    x(0).store.add(or(x,z))
    z
  }  
  


  def table(x: Array[CPIntVar], tuples: Array[Array[Int]]): Constraint = {
    //new TableSTR2(x,tuples)
    
    import oscar.cp.constraints.TableAC5TCRecomp
    val data = new TableData(x.size)
    tuples.foreach(t => data.add(t: _*))
    new oscar.cp.constraints.TableAC5TCRecomp(data, x: _*)
    
    /*
    val tab = new TableJava(x:_*)
    tuples.foreach(t => tab.addTupple(t:_*))
    return tab
  	*/
  }

  /**
   * Table Constraints for couples (constraint given in extension by enumerating valid assignments)
   * @param x1 first variable
   * @param x2 second variable
   * @param tuples a collection of coulples
   * @return a constraint enforcing that (x1,x2) is one of the couples given in tuples
   */
  def table(x1: CPIntVar, x2: CPIntVar, tuples: Iterable[(Int, Int)]): Constraint = {
    table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,tuples)

  }

  /**
   * Table Constraints for triples (constraint given in extension by enumerating valid assignments)
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param tuples a collection of triples
   * @return a constraint enforcing that (x1,x2,x3) is one of the triples given in tuples
   */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, tuples: Iterable[(Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,tuples)
  }

  /**
   * Table Constraints for quadruples
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param x4 fourth variable
   * @param tuples a collection of quadruples
   * @return a constraint enforcing that (x1,x2,x3,x4) is one of the quadruples given in tuples
   */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, tuples: Iterable[(Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,x4,tuples)
  }

  /**
   * Table Constraints for quadruples
   * @param x1 first variable
   * @param x2 second variable
   * @param x3 third variable
   * @param x4 fourth variable
   * @param x5 fifth variable
   * @param tuples a collection of five-tuples
   * @return a constraint enforcing that (x1,x2,x3,x4,x5) is one of the five-tuples given in tuples
   */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, x5: CPIntVar, tuples: Iterable[(Int, Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
    //import oscar.cp.constraints.TableAC5TCRecomp
    //new oscar.cp.constraints.TableAC5TCRecomp(x1,x2,x3,x4,tuples)
  }

  def modulo(x: CPIntVar, v: Int, y: CPIntVar): Constraint = {
    return new Modulo(x, v, y)
  }

  def modulo(x: CPIntVar, v: Int, y: Int): Constraint = {
    return new Modulo(x, v, CPIntVar(y)(x.store))
  }
  
 
  /**
   * Among Constraint: n is the number of variables from x in set s.
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that n = #{ i | x(i) in s }
   */  
  def among(n: CPIntVar, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    new Among(n,x,s)
  }
  
  /**
   * AtLeast Constraint: at least n variables take their value in s
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that  #{ i | x(i) in s } >= n
   */  
  def atLeast(n: Int, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    among(CPIntVar(n, x.size)(x(0).store),x,s)
  }

  /**
   * AtLeast Constraint: at least n variables equal to v
   * @param n counter variable
   * @param x array of variables
   * @param v a value
   * @return a constraint enforcing that  #{ i | x(i) = v } >= n
   */  
  def atLeast(n: Int, x: IndexedSeq[CPIntVar], v: Int): Constraint = atLeast(n,x,Set(v)) 
  
  /**
   * AtMost Constraint: at most n variables take their value in s
   * @param n counter variable
   * @param x array of variables
   * @param s set of values
   * @return a constraint enforcing that  #{ i | x(i) in s } <= n
   */  
  def atMost(n: Int, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    among(CPIntVar(0, n)(x(0).store),x,s)
  }

  /**
   * AtMost Constraint: at least n variables equal to v
   * @param n counter variable
   * @param x array of variables
   * @param v a value
   * @return a constraint enforcing that  #{ i | x(i) = v } <= n
   */  
  def atMost(n: Int, x: IndexedSeq[CPIntVar], v: Int): Constraint = atMost(n,x,Set(v))
  
  /**
   * Count Constraint: n is the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n = #{ i | x(i) = y }
   */  
  def countEq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar): Constraint = {
    if (y.isBound) countEq(n,x,y.value)
    else new CountSimple(n,x,y)
  }
  
  def countEq(n: CPIntVar, x: IndexedSeq[CPIntVar], v: Int) = {
    new CountCst(n,x,v)
  }  
  
  /**
   * Count Constraint: n is greater or equal to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n >= #{ i | x(i) = y }
   */  
  def countGeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    val ok = n.store.post(n >= c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is greater than the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n > #{ i | x(i) = y }
   */  
  def countGt(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    val ok = n.store.post(n > c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is less or equal to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n <= #{ i | x(i) = y }
   */  
  def countLeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    val ok = n.store.post(n <= c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }
  
  /**
   * Count Constraint: n is less than the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n <= #{ i | x(i) = y }
   */  
  def countLt(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    val ok = n.store.post(n < c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }  
  
  /**
   * Count Constraint: n is different to the number of variables from x equal to y.
   * @param n is a counter variable
   * @param x is an array of variables
   * @param y is a variable
   * @return a constraint enforcing that n != #{ i | x(i) = y }
   */  
  def countNeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    val ok = n.store.post(n != c)
    assert(ok != CPOutcome.Failure)
    new Count(c,x,y)
  }   
  
  /**
   * Global Cardinality Constraint: every value occurs at least min and at most max
   * @param x an non empty array of variables
   * @param values is the range of constrained values
   * @param min is the minimum number of occurrences for each value in the range values
   * @param max is the maximum number of occurrences for each value in the range values
   * @return a constraint such that each value in the range values occurs at least min and at most max times.
   */
  def gcc(x: Array[CPIntVar], values: Range, min: Int, max: Int): Constraint = {
    return new GCC(x, values.min, Array.fill(values.size)(min), Array.fill(values.size)(max))
  }

  /**
   * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
   * @param x an non empty array of variables
   * @param values is the range of constrained values
   * @param min is the minimum number of occurrences for each value in the range values
   * @param max is the maximum number of occurrences for each value in the range values
   * @return a constraint such that each value in the range values occurs at least min and at most max times.
   */
  def gcc(x: Array[CPIntVar], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    return new GCC(x, values.min, min, max)
  }

  /**
   * Soft Global Cardinality Constraint = gcc with a violation variable
   * @see Revisiting the Soft Global Cardinality Constraint, Pierre Schaus, Pascal Van Hentenryck, Alessandro Zanarini: CPAIOR 2010
   */
  def softGcc(x: Array[CPIntVar], values: Range, min: Array[Int], max: Array[Int], viol: CPIntVar): SoftGCC = {
    return new SoftGCC(x, values.min, min, max, viol)
  }

  /**
   * Global Cardinality Constraint with variable counters
   * @param x a non empty array of variables
   * @param valueOccurence is an array of pairs (v,o)
   *        where o is variable representing the number of occurrences of value v
   * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
   */
  def gcc(x: IndexedSeq[CPIntVar], valueOccurrence: Iterable[(Int,CPIntVar)]): Constraint = {
    def freshCard(): CPIntVar = CPIntVar(0, x.size - 1)(x.head.store)
    val sortedValOcc = valueOccurrence.toArray.sortWith((a, b) => a._1 <= b._1)
    val (v0,x0) = sortedValOcc(0)
    var values = Array(v0)
    var cardinalities = Array(x0)
    for (i <- 1 until sortedValOcc.length) {
      val (vi_1,xi_1) = sortedValOcc(i - 1)
      val (vi,xi) = sortedValOcc(i)
      for (v <- (vi_1 + 1) until vi) {
        values = values :+ v
        cardinalities = cardinalities :+ freshCard()
      }
      values = values :+ vi
      cardinalities = cardinalities :+ xi
    }
    new GCCVar(x.toArray, values(0), cardinalities)
  }

  
  def gcc(x: Array[CPIntVar], valueOccurrence: Array[(Int,CPIntVar)]): Constraint = {
    gcc(x.toIndexedSeq, valueOccurrence.toIterable)
  }

  // regular and automatons

  /**
   * @see stretchAutomaton(Array[CPIntVar],Int,Int,Iterable[Tuple2[Int, Int]])
   */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Int, maxStretch: Int): Automaton = {
    stretchAutomaton(vars, minStretch, maxStretch, None)
  }

  /**
   * Builds an automaton restricting the number consecutive times the values appear.
   * A stretch is a consecutive number of a same value in vars for instance 112223335 start with a stretch of length 2 of value 1, followed by a stretch of length 3 with value 2,
   * followed by a stretch of length 3 of value 3 followed by a stretch of 1 of value 5.
   * @param vars an non empty array of variables
   * @param minStretch the minimum stretch length for any value
   * @param maxStretch the maximum stretch length for any value
   * @return an automaton
   */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Int, maxStretch: Int, transitions: Iterable[Tuple2[Int, Int]]): Automaton = {
    val maxv = vars.map(x => x.getMax).max
    val minv = vars.map(x => x.getMin).min
    if (minv < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    val minimumStretch = Array.tabulate(maxv + 1)(_ => minStretch)
    val maximumStretch = Array.tabulate(maxv + 1)(_ => maxStretch)

    stretchAutomaton(vars, minimumStretch, maximumStretch, transitions)
  }

  /**
   * @see stretchAutomaton(Array[CPIntVar],Int,Int,Iterable[Tuple2[Int, Int]])
   */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Array[Int], maxStretch: Array[Int], transitions: Iterable[Tuple2[Int, Int]] = None): Automaton = {
    val maxv = vars.map(x => x.getMax).max
    val minv = vars.map(x => x.getMin).min
    if (minv < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    if (transitions != None && !transitions.isEmpty) {
      var transiFrom = Array[Int]()
      var transiTo = Array[Int]()
      transitions.foreach(t => { transiFrom = transiFrom :+ t._1; transiTo = transiTo :+ t._2 })
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch, transiFrom, transiTo)
    } else {
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch)
    }
  }

  /**
   * Regular Constraint, ensuring that vars accepted by a context free grammar described by given automaton
   * @param vars an non empty array of variables, with domains belonging to the set of transitions of the automaton
   * @param automaton a deterministic automaton
   * @return a constraint ensuring that m is the maximum of variables in vars
   */
  def regular(vars: Array[CPIntVar], automaton: Automaton): Constraint = {
    new Regular(vars, automaton)
  }

  /**
   * Maximum Constraint
   * @param indexes
   * @param f function mapping each element from indexes to a variable
   * @return a fresh variable z linked to vars by a constraint such that z is the maximum of all variables f(A) for all A in indexes
   */
  def maximum[A](indexes: Iterable[A])(f: A => CPIntVar): CPIntVar = maximum(indexes map f)

  /**
   * Maximum Constraint
   * @param vars an non empty array of variables
   * @param m a variables representing the maximum of vars
   * @return a constraint ensuring that m is the maximum of variables in vars
   */
  def maximum(vars: Array[CPIntVar], m: CPIntVar): Constraint = {
    new Maximum(vars, m)
  }

  /**
   * Maximum Constraint
   * @param vars an non empty array of variables
   * @return a fresh variable z linked to vars by a constraint such that z is the maximum of all variables in vars
   */
  def maximum(vars: Iterable[CPIntVar]): CPIntVar = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPIntVar(vars.map(_.min).max, vars.map(_.max).max)(cp)
    cp.add(maximum(x, m))
    m
  }

  /**
   * Minimum Constraint
   * @param indexes
   * @param f function mapping each element from indexes to a variable
   * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables f(A) for all A in indexes
   */
  def minimum[A](indexes: Iterable[A])(f: A => CPIntVar): CPIntVar = minimum(indexes map f)

  /**
   * Minimum Constraint
   * @param vars an non empty array of variables
   * @param m a variables representing the maximum of vars
   * @return a constraint ensuring that m is the minimum of variables in vars
   */
  def minimum(vars: Array[CPIntVar], m: CPIntVar): Constraint = {
    new Minimum(vars, m)
  }

  /**
   * Minimum Constraint
   * @param vars an non empty array of variables
   * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables in vars
   */
  def minimum(vars: Iterable[CPIntVar]): CPIntVar = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPIntVar(vars.map(_.min).min, vars.map(_.max).min)(cp)
    cp.add(minimum(x, m))
    m
  }

  /**
   * Constraint enforcing n * sum,,i,, |x[i]-s/n| <= nd and sum,,i,, x[i] = s <br>
   * Note that this constraint is very similar spread.
   * @param x
   * @param s
   * @param nd
   */
  def deviation(x: Iterable[CPIntVar], s: Int, nd: CPIntVar): Constraint = {
    new Deviation(x.toArray, s, nd)
  }

  /**
   * Constraint enforcing sum,,i,, x[i]^2^ <= s2 and sum,,i,, x[i] = s <br>
   * Note that this constraint is very similar deviation.
   * @param x
   * @param s
   * @param s2
   */
  def spread(x: Iterable[CPIntVar], s: Int, s2: CPIntVar): Constraint = {
    new Spread(x.toArray, s, s2, true)
  }

  /**
   * Let n = x.size-1 = y.size-1
   * This constraint enforces that x and y are permutations over {0, ... , n}
   * with y(i) giving the position of number i in x. It means that x(y(i)) = i
   * Note that this constraint could be enforced with element constraints but it is less efficient
   * Weak and Strong consistency can be used acting on the filtering of alldifferent constraints
   * @param x
   * @param y of same size as x
   */
  def permutation(x: IndexedSeq[CPIntVar], y: IndexedSeq[CPIntVar]): Constraint = {
    new Permutation(x, y)
  }

  def sortedness(x: IndexedSeq[CPIntVar], s: IndexedSeq[CPIntVar], p: IndexedSeq[CPIntVar],strictly: Boolean = false): LinkedList[Constraint] = {
    val cp = x(0).store
    val n = x.size
    val cons = new LinkedList[Constraint]
    for (i <- 0 until n - 1) {
      cons.add(elementVar(x, p(i),Strong) <= elementVar(x, p(i + 1),Strong))
      if (strictly) {
        cons.add(s(i) < s(i + 1))
      } else {
        cons.add(s(i) <= s(i + 1))
      }
      
    }
    val minx = x.map(_.min).min
    val maxx = x.map(_.max).max
    val mins = s.map(_.min).min
    val maxs = s.map(_.max).max
    
    for( i <- 0 until x.size) {
      cons.add(p(i) >= 0)
      cons.add(p(i) <= n)
      
      cons.add(s(i) <= maxx)
      cons.add(s(i) >= minx)
      
      cons.add(x(i) <= maxs)
      cons.add(x(i) >= mins)
    }
    for (i <- 0 until n) {
      cons.add(elementVar(x, p(i), s(i)))
    }
    cons.add(allDifferent(p))

    val minVal: Int = x.map(_.min).min
    val maxVal: Int = x.map(_.max).max

    // array of variable occ with domains {0,...,n} that will represent the number of occurrences of each value
    val occ = Array.fill(maxVal - minVal + 1)(CPIntVar(0 to n)(cp))
    cons.add(gcc(x, (minVal to maxVal).zip(occ)))

    // nbBefore(i) = #{i | x(i) < i } i.e. number of values strictly small than i for i in [minVal .. maxVal]
    val nbBefore = for (i <- minVal to maxVal) yield {
      if (i == minVal) CPIntVar(0)(cp)
      else sum(minVal to i - 1)(j => occ(j))
    }

    for (i <- 0 until n) {
      // there are less than i values smaller than s(i) 
      cons.add(elementVar(nbBefore, s(i) - minVal) <= i)
    }
    cons
  }

  /**
   * The StockingCost constraint holds when each item is produced before
   * its due date ($X_i <= d_i$), at most one item is produced at any time
   * on the machine (all the $X_i$ are distinct), and $H$
   * is an upper bound on the total stocking cost ($sum_i(d_i - X_i) <= H$).
   * 
   * This constraint is useful for modeling
   * Production Planning Problem such as Lot Sizing Problems
   * 
   * @param X, the variable $X_i$ is the date of production of item $i$ on the machine
   * @param d, the integer $d_i$ is the due-date for item $i$
   * @param H, the variable $H$ is an upper bound on the total number of slots all the items are need in stock.
   */
  def stockingCost(X: Array[CPIntVar], d: Array[Int], H: CPIntVar): Constraint = {
    new StockingCost(X, d, H, 1)
  }


  /**
   * Non overlapping between 2D rectangles 
   * @param x is the x coordinates of the bottom left corner of rectangles
   * @param dx is the length in direction of x of each rectangle
   * @param y is the y coordinates of the bottom left corner of rectangles
   * @param dy is the length in direction y of each rectangle
   * @return a set of constraints such that posting all of them enforces the non overlapping of rectangles
   */   
  def diffn(x: Array[CPIntVar], dx: Array[CPIntVar], y: Array[CPIntVar], dy: Array[CPIntVar]): Iterable[Constraint] = {
    val endx = Array.tabulate(x.size)(i => x(i) + dx(i))
    val endy = Array.tabulate(y.size)(i => y(i) + dy(i))
    val capay = maximum(endy) - minimum(y)
    val capax = maximum(endx) - minimum(x)
    var cons = Vector[Constraint]()
    for (i <- 0 until x.length; j <- i + 1 until x.length) {
      cons = cons :+ (new Or(Array(x(i) + dx(i) <== x(j),
        x(j) + dx(j) <== x(i),
        y(i) + dy(i) <== y(j),
        y(j) + dy(j) <== y(i),
        x(i) + dx(i) <== x(j),
        x(j) + dx(j) <== x(i),
        y(i) + dy(i) <== y(j),
        y(j) + dy(j) <== y(i))));
    }
    cons = cons :+ (maxCumulativeResource(x, dx, endx, dy, capay))
    cons = cons :+ (maxCumulativeResource(y, dy, endy, dx, capax))
    return cons
  }
  

  
  // scheduling constraints


  /**
   * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with required(i) = true) can overlap in time 
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param required tells if a task is scheduled on this resource or not, if not this task is not constrained
   */  
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], required: Array[CPBoolVar]): UnaryResource = {
	new UnaryResource(starts,durations,ends,required)
  }
  

  /**
   * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with resources(i) = id) can overlap in time 
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param resources the variables representing the resource where the task is scheduled
   * @param id, the resource on which we want to constraint, tasks i such that resources(i) != id are not considered
   */  
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int) = {
	new UnaryResource(starts,durations,ends,resources,id)
  }

  /**
   * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   */    
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]) = {
    val cp = starts(0).store
	new UnaryResource(starts,durations,ends,starts.map(s => CPBoolVar(true)(cp)))
  }
  

  /**
   * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks executing on the resource id, must be <= than the capacity
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param demands the variables representing how much each task consume of the resource
   * @param resources the variables representing the resource where the task is scheduled
   * @param capacity the capacity of the resource
   * @param id, the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
   */ 
  def maxCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    MaxCumulative(starts,durations,ends,demands,resources,capacity,id)
  }
  
  /**
   * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks must be <= than the capacity
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param demands the variables representing how much each task consume of the resource
   * @param capacity the capacity of the resource
   */ 
  def maxCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar): Constraint = {
    val cp = starts(0).store
    val resources = Array.fill(starts.size)(CPIntVar(0)(cp))
    maxCumulativeResource(starts,durations,ends,demands,resources,capacity,0)
  }   
  
  /**
   * Discrete Resource constraint with minimum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks executing on the resource id, must be >= than the capacity
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param demands the variables representing how much each task consume of the resource
   * @param resources the variables representing the resource where the task is scheduled
   * @param capacity the capacity of the resource
   * @param id, the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
   */ 
  def minCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    new SweepMinCumulative(starts: Array[CPIntVar],durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int)
  }
  
  /**
   * Discrete Resource constraint with maximum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks must be >= than the capacity
   * @param starts the variables representing the start time of the tasks
   * @param durations the variables representing the duration of the tasks
   * @param ends the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
   * @param demands the variables representing how much each task consume of the resource
   * @param capacity the capacity of the resource
   */ 
  def minCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar): Constraint = {
    val cp = starts(0).store
    val resources = Array.fill(starts.size)(CPIntVar(0)(cp))
    minCumulativeResource(starts,durations,ends,demands,resources,capacity,0)
  }    
  

  
  /**
   * Constraint x and y to be disjoint (no common values)
   * @param x:
   * @param y:
   */
  def disjoint(x: CPSetVar, y: CPSetVar): Constraint = new Disjoint(x,y)
}
