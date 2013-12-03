package oscar.cbls.invariants.tests

import scala.collection.immutable.SortedSet
import collection.immutable.SortedMap
import org.scalacheck.Gen
import org.scalacheck.Gen.value
import org.scalacheck.Prop
import org.scalatest.prop.Checkers
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic._
import org.scalatest.FunSuite
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.minmax.MinLin
import oscar.cbls.invariants.lib.set.MakeSet
import oscar.cbls.invariants.lib.minmax.MinSet
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.set.Inter
import oscar.cbls.invariants.lib.set.SetSum
import oscar.cbls.constraints.lib.global.Sequence
import oscar.cbls.invariants.lib.logic.IntITE
import oscar.cbls.invariants.lib.numeric.Prod
import scala.Some
import oscar.cbls.invariants.lib.set.Interval
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.lib.set.SetProd
import oscar.cbls.invariants.lib.minmax.ArgMinArray
import oscar.cbls.invariants.lib.minmax.MaxLin
import oscar.cbls.invariants.lib.numeric.ProdElements
import oscar.cbls.invariants.lib.minmax.Min2
import oscar.cbls.invariants.lib.numeric.Prod2
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.logic.SelectLEHeapHeap
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.constraints.lib.global.AtMost
import oscar.cbls.invariants.lib.numeric.Abs
import oscar.cbls.invariants.lib.minmax.Max
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.numeric.Step
import oscar.cbls.invariants.lib.logic.IntSetElement
import oscar.cbls.invariants.lib.logic.IntElements
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.invariants.lib.set.Union
import oscar.cbls.invariants.lib.minmax.Min
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.logic.DenseCount
import oscar.cbls.constraints.lib.global.AtLeast
import oscar.cbls.invariants.lib.set.Diff
import oscar.cbls.invariants.lib.set.TakeAny
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.MaxSet
import oscar.cbls.invariants.lib.logic.SelectLESetQueue

class smalltest extends FunSuite with Checkers{

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvariantTestBench
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }


}

class InvariantTests extends FunSuite with Checkers{

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvariantTestBench
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }


  test("AtLeast") {
    val bench = new InvariantTestBench
    new AtLeast(bench.genIntVars(10), bench.genBoundedValues(10, 0 to 30, 0 to 10)).toIntVar
    bench.run
  }

  test("AtMost") {
    val bench = new InvariantTestBench
    new AtMost(bench.genIntVars(10), InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)).toIntVar
    bench.run
  }

  test("MultiKnapsack") {
    val bench = new InvariantTestBench
    //E elements, B bins
    val e:Int = 10
    val b:Int = 5
    new MultiKnapsack(
      bench.genIntVarsArray(e,0 to (b-1)),
      bench.genIntVarsArray(e,0 to 100),
      bench.genIntVarsArray(b,0 to 200)).toIntVar
    bench.run
  }

  test("Sequence") {
    val bench = new InvariantTestBench
    new Sequence(
      bench.genIntVarsArray(20,0 to 10),
      13,
      5,
      (x: Int) => x > 1).toIntVar
    bench.run
  }

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvariantTestBench
    new IntITE(bench.genIntVar(-2 to 3), bench.genIntVar(1 to 2), bench.genIntVar(10 to 11)).toIntVar
    bench.run
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvariantTestBench
    new IntElement(bench.genIntVar(0 to 19), bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvariantTestBench
    new IntElements(bench.genIntSetVar(3, 0 to 4), bench.genIntVarsArray(5, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvariantTestBench
    new IntSetElement(bench.genIntVar(0 to 19), bench.genIntSetVars(20, 10, 0 to 100)).toIntSetVar
    bench.run
  }

  test("Sparse Cluster maintains a cluster of the indexes of an array.") {
    val bench = new InvariantTestBench
    Cluster.MakeSparse(bench.genIntVarsArray(50),
      (Gen.containerOfN[List, Int](100, Gen.choose(0, 100))).sample.get)
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array.") {
    val bench = new InvariantTestBench
    Cluster.MakeDense(bench.genIntVarsArray(50))
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array"
    + " (assuming min and max).") {
    val bench = new InvariantTestBench
    Cluster.MakeDenseAssumingMinMax(bench.genIntVarsArray(50), 0, 100)
    bench.run
  }

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvariantTestBench
    new DenseCount(bench.genIntVarsArray(10, 0 to 19), bench.genIntVarsArray(20, 0 to 19, false))
    bench.run
  }

  ignore("Cross references...")(pending)

  ignore("Cumulative...")(pending)

  test("Filter..."){
    val bench = new InvariantTestBench
    new Filter(bench.genIntVarsArray(4, 0 to 5), (i:Int) => (i % 2) == 0).toIntSetVar
    bench.run
  }

  test("SelectLEHeapHeap") {
    val bench = new InvariantTestBench
    new SelectLEHeapHeap(bench.genIntVarsArray(4, 0 to 5), bench.genIntVar(3 to 10)).toIntSetVar
    bench.run
  }

  ignore("SelectLESetQueue") {
  //TODO exclure les changements de valeurs interdits
    //le pivot ne peut qu'augmenter
    //une valeur en dessous du pivot ne peut que prendre une valeur dépassant toutes les autres valeurs
    //les valeurs au dessus du pivot ne peuvent pas changer
    val bench = new InvariantTestBench
    new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
    bench.run
  }

  ignore("Predecessor")(pending)

  ignore("Routes")(pending)

  //this is not working so far.
  test("Sort") {
    val bench = new InvariantTestBench
    Sort.MakeSort(bench.genIntVarsArray(4, 0 to 30))
    bench.run
  }

  test("ArgMinArray") {
    val bench = new InvariantTestBench
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  test("ArgMinArray and playing with the index") {
    val bench = new InvariantTestBench
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30),bench.genIntSetVar(1,0 to 19)).toIntSetVar
    bench.run
  }

  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvariantTestBench
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  test("ArgMaxArray and playing with the Index") {
    val bench = new InvariantTestBench
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30),bench.genIntSetVar(1,0 to 19)).toIntSetVar
    bench.run
  }

  test("MaxLin") {
    val bench = new InvariantTestBench
    new MaxLin(bench.genSortedIntVars(6, -10 to 10)).toIntVar
    bench.run
  }

  test("MinLin") {
    val bench = new InvariantTestBench
    new MinLin(bench.genSortedIntVars(6, 0 to 10)).toIntVar
    bench.run
  }

  test("Min") {
    val bench = new InvariantTestBench
    new Min(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Max") {
    val bench = new InvariantTestBench
    new Max(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Min2") {
    val bench = new InvariantTestBench
    new Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("Max2") {
    val bench = new InvariantTestBench
    new Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("MinArray maintains the minimum from an array of variables.") {
    val bench = new InvariantTestBench
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    bench.run
  }

  test("MaxArray maintains the maximum from an array of variables.") {
    val bench = new InvariantTestBench
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    bench.run
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvariantTestBench
    new MinSet(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("MaxSet maintains the maximum of a set") {
    val bench = new InvariantTestBench
    new MaxSet(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("SumElements maintains the sum of variables of which indices are in the given set.") {
    val bench = new InvariantTestBench
    new SumElements(bench.genIntVarsArray(10, 0 to 100), bench.genIntSetVar(5, 0 to 9)).toIntVar
    bench.run
  }

  test("ProdElements maintains the product of variables of which indices are in the given set.") {
    val bench = new InvariantTestBench
    new ProdElements(bench.genIntVarsArray(5, 0 to 10), bench.genIntSetVar(2, 0 to 4)).toIntVar
    bench.run
  }

  test("Sum maintains the sum of input variables.") {
    val bench = new InvariantTestBench
    new Sum(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }

  test("Prod maintains the product of input variables.") {
    val bench = new InvariantTestBench
    new Prod(bench.genIntVarsArray(3, 0 to 100)).toIntVar
    bench.run
  }

  test("Minus maintains the difference between two variables.") {
    val bench = new InvariantTestBench
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Sum2 maintains the sum of two variables.") {
    val bench = new InvariantTestBench
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Prod2 maintains the product of two variables") {
    val bench = new InvariantTestBench
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Div maintains the division of two variables.") {
    val bench = new InvariantTestBench
    new Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
    bench.run
  }

  test("Mod maintains the modulo of two variables.") {
    val bench = new InvariantTestBench
    new Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
    bench.run
  }

  test("Abs maintains the absolute value of a variable.") {
    val bench = new InvariantTestBench
    new Abs(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("Step maintains a step function of the input var.") {
    val bench = new InvariantTestBench
    new Step(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  ignore("RoundUpModulo")(pending)

  ignore("RoundUpCustom")(pending)

  test("Union maintains the union of two sets.") {
    val bench = new InvariantTestBench
    new Union(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Inter maintains the intersection of two sets.") {
    val bench = new InvariantTestBench
    new Inter(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Diff maintains the difference between two sets.") {
    val bench = new InvariantTestBench
    new Diff(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Cardinality maintains the cardinality of a set.") {
    val bench = new InvariantTestBench
    new Cardinality(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("MakeSet maintains an IntSetVar given a set of IntVar.") {
    val bench = new InvariantTestBench
    new MakeSet(bench.genSortedIntVars(10, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Interval maintains the set in the interval.") {
    val bench = new InvariantTestBench
    new Interval(bench.genIntVar(-100 to 100), bench.genIntVar(-100 to 100)).toIntSetVar
    bench.run
  }

  test("TakeAny maintains a value taken from the set.") {
    val bench = new InvariantTestBench
    new TakeAny(bench.genIntSetVar(), 0).toIntVar
    bench.run
  }

  test("SetSum maintains the sum of variables (after optionnaly appliying a function).") {
    val bench = new InvariantTestBench
    new SetSum(bench.genIntSetVar()).toIntVar
    bench.run
  }

  /**
   * Won't pass when the product products an overflow.
   */
  test("SetProd maintains the product of variables (after optionnaly appliying a function).") {
    val bench = new InvariantTestBench
    new SetProd(bench.genIntSetVar(10, -3 to 3)).toIntVar
    bench.run
  }
}

/**
 * This class represents a move in the model, that is, one or several
 * modifications of the variables of the model.
 *
 * We distinguish between some identified "extremum" moves which can be used
 * as well on IntVar as on IntSetVar.
 */
abstract class Move

case class PlusOne extends Move
case class MinusOne extends Move
case class ToZero extends Move
case class ToMin extends Move
case class ToMax extends Move
case class Random extends Move
case class RandomDiff extends Move

/**
 * This object contains a set of functions and methods to generate random
 * moves and variables, which we need for the tests.
 */
object InvGen {
  /**
   * Function to generate a random move.
   */
  val move = Gen.oneOf(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
    Random(), RandomDiff())

  def randomIntSortedMap(nbVal: Int, valRange: Range, boundRange: Range): SortedMap[Int, Int] = {
    val valList = Gen.containerOfN[List, Int](nbVal,
      Gen.choose(valRange.min, valRange.max).sample.get).sample.get
    val map = valList.map((value: Int) => (
      value, Gen.choose(boundRange.min, boundRange.max).sample.get))
    SortedMap(map: _*)
  }

  /**
   * Method to generate a random IntVar:
   * - a random value which satisfies the given constraint is chosen in the
   * given range
   * - a random lower case character is chosen to be used
   *  as the name of the variable
   * The generated variable is added to the given model.
   */
  def randomIntVar(range: Range, model: Model, constraint: Int => Boolean) =
    for {
      v <- Gen.choose(range.min, range.max) suchThat (constraint(_))
      c <- Gen.alphaChar
    } yield new RandomIntVar(
      new IntVar(model, range, v, c.toString.toLowerCase), constraint)

  /**
   * Method to generate a list of nbVars random IntVar. Uses randomIntVar
   * method to generate each variable.
   */
  def randomIntVars(nbVars: Int, range: Range, model: Model, constraint: Int => Boolean) = {
    Gen.containerOfN[List, RandomIntVar](nbVars, randomIntVar(range, model, constraint))
  }

  /**
   * Method to generate a random IntSetVar of given size:
   * - a list of nbVars random values are chosen in the given range
   * - a random upper case character is chosen to be used
   *  as the name of the variable
   * A sorted set is made of the list of values, and the generated variable
   * is added to the given model.
   */
  def randomFixedIntSetVar(nbVars: Int, range: Range, model: Model) = for {
    c <- Gen.alphaChar
    v <- Gen.containerOfN[List, Int](nbVars, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(
    new IntSetVar(model, range.min, range.max, c.toString.toUpperCase,
      SortedSet(v: _*)))

  /**
   * Method to generate a random IntSetVar of size less or equal to the given
   * limit. Same as randomFixedIntSetVar, except the size is chosen randomly.
   */
  def randomIntSetVar(upToSize: Int, range: Range, model: Model) = for {
    c <- Gen.alphaChar
    s <- Gen.choose(1, upToSize)
    v <- Gen.containerOfN[List, Int](s, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(new IntSetVar(model, range.min, range.max,
    c.toString.toUpperCase, SortedSet(v: _*)))

  /**
   * Method to generate a list of IntSetVars. Uses randomIntSetVar.
   */
  def randomIntSetVars(nbVars: Int, upToSize: Int, range: Range, model: Model) = {
    Gen.containerOfN[List, RandomIntSetVar](nbVars,
      randomIntSetVar(upToSize, range, model))
  }
}

/**
 * A RandomVar contains a variable which can be modified using a Move.
 */
abstract class RandomVar {
  def randomVar(): Variable

  def move(move: Move)

  override def toString = randomVar.toString
}

/**
 * A RandomIntVar is a RandomVar containing an IntVar.
 * It can also contains a constraint which is applied when the variable is
 * moving.
 */
case class RandomIntVar(intVar: IntVar,
  constraint: Int => Boolean = (v: Int) => true) extends RandomVar {

  override def randomVar(): IntVar = intVar

  def applyConstraint(newVal: Int) {
    if (constraint(newVal)) {
      randomVar := newVal
    }
  }

  /**
   * Defines the different possible moves for a RandomIntVar. Most are quite
   * obvious: PlusOne applies +1 to the IntVar value, ToZero sets the value
   * to zero, ToMax sets the value to the max value of the variable range.
   * Random sets the value to a randomly chosen one (in the variable range).
   * RandomDiff sets the value to a randomly chosen one, but different from
   * the previous one.
   */
  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        val newVal = randomVar.value + 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.minVal)
      }
      case MinusOne() => {
        val newVal = randomVar.value - 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.maxVal)
      }
      case ToZero() => {
        applyConstraint(0)
      }
      case ToMax() => {
        applyConstraint(randomVar.maxVal)
      }
      case ToMin() => {
        applyConstraint(randomVar.minVal)
      }
      case Random() => {
        applyConstraint(Gen.choose(randomVar.minVal, randomVar.maxVal).sample.get)
      }
      case RandomDiff() => {
        val randomOpt = (Gen.choose(randomVar.minVal, randomVar.maxVal)
          suchThat (_ != randomVar.value)).sample
        if (randomOpt.isDefined) applyConstraint(randomOpt.get)
      }
    }
  }
}

/**
 * A RandomIntSetVar is a RandomVar containing an IntSetVar.
 */
case class RandomIntSetVar(intSetVar: IntSetVar) extends RandomVar {
  override def randomVar(): IntSetVar = intSetVar

  /**
   * Defines the different possible moves for a RandomIntSetVar.
   * PlusOne adds a new random value to the set whereas MinusOne removes one,
   * ToZero makes the set an empty one, ToMax adds all the values of the
   * variable range to the set whereas ToMin makes the set a singleton (of
   * which value is randomly chosen).
   * Random replaces the set with a random one (values and size are random)
   * but to avoid explosions, new size cannot be more than current size + 1.
   * RandomDiff replaces it with a random one with which intersection is empty,
   * if such a change is not possible, nothing's done.
   */
  override def move(move: Move) = {
    move match {
      case PlusOne() => { // Adds an element to the set
        randomVar :+= Gen.choose(randomVar.getMinVal, randomVar.getMaxVal).sample.get
      }
      case MinusOne() => { // Removes an element from the set
        if (!randomVar.value.isEmpty) randomVar :-= Gen.oneOf(randomVar.value.toSeq).sample.get
        //else randomVar.value = Seq.fill(randomVar.value.size)(util.Random.nextInt)
      }
      case ToZero() => { // Removes all elements from the set
        randomVar.value.foreach(value => randomVar.deleteValue(value))
      }
      case ToMax() => { // Adds all elements between min and max to the set
        (randomVar.getMinVal to randomVar.getMaxVal).foreach(v => randomVar :+= v)
      }
      case ToMin() => { // Reduces the set to a singleton
        randomVar.value.foreach(value => randomVar.deleteValue(value))
        randomVar :+= Gen.choose(randomVar.getMinVal, randomVar.getMaxVal).sample.get
      }
      case Random() => { // Replaces the set with a randomly generated one
        val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newVal = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.getMinVal, randomVar.getMaxVal)).sample.get
        randomVar := SortedSet(newVal: _*)
      }
      case RandomDiff() => {
        // Replaces the set with a randomly generated one
        // with which intersection is empty
        val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newValOpt = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.getMinVal, randomVar.getMaxVal)
            suchThat (!randomVar.value.contains(_))).sample
        if (newValOpt.isDefined) randomVar := SortedSet(newValOpt.get: _*)
      }
    }
  }
}

/**
 * This class is intended to be used as a test bench for an invariant.
 * It contains a property which is : "Given a model, for any move applied to
 * its variables, its invariants hold.". In practice, we create a model with
 * only one invariant, generate most possible extreme moves of its
 * variables, and check this invariant at each move.
 *
 * When the invariant is created, we distinguish between input variables on
 * which moves can be applied, and output variables which will be updated by
 * the invariant only.
 *
 * Its argument 'verbose' is for debug messages printing :
 * 0 (or less) for no debug
 * 1 for a minimum debug
 * 2 (or more) for total debug
 */
class InvariantTestBench(verbose: Int = 0) {
  var property: Prop = false
  val checker = new InvariantChecker(verbose)
  val model = new Model(false, Some(checker),NoCycle = true)
  var inputVars: List[RandomVar] = List()
  var outputVars: List[RandomVar] = List()

  /**
   * These methods add variables to the bench.
   * input is true if the variable is an input variable, and false if it is an
   * output variable.
   */
  def addVar(input: Boolean, v: RandomVar) {
    addVar(input, List(v))
  }

  def addVar(input: Boolean, vars: Iterable[RandomVar]) {
    for (v <- vars) {
      if (input) inputVars = v :: inputVars
      else outputVars = v :: outputVars
    }
  }

  /**
   * Method for generating a new random IntVar to add to the bench and to its
   * model.
   */
  def genIntVar(
    range: Range,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): IntVar = {
    val riVar = InvGen.randomIntVar(range, model, constraint).sample.get
    addVar(isInput, riVar)
    riVar.randomVar
  }

  /**
   * Method for generating an array of random IntVar to add to the bench and to its
   * model.
   */
  def genIntVars(
    nbVars: Int = 4,
    range: Range = 0 to 100,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): List[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    riVars.map((riv: RandomIntVar) => {
      riv.randomVar
    })
  }

  def genIntVarsArray(
    nbVars: Int = 4,
    range: Range = 0 to 100,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): Array[IntVar] = {
    genIntVars(nbVars, range, isInput, constraint).toArray
  }

  /**
   * Method for generating a sorted set of random IntVar to add to the bench
   * and to its model.
   */
  def genSortedIntVars(
    nbVars: Int,
    range: Range,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): SortedSet[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    val iVars = riVars.map((riv: RandomIntVar) => { riv.randomVar })
    SortedSet(iVars: _*)
  }

  def genBoundedValues(
    nbVars: Int,
    rangeValue: Range,
    rangeBound: Range,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): SortedMap[Int, IntVar] = {
    val boundVars = genIntVars(nbVars, rangeBound, isInput, constraint)
    val map = boundVars.map((boundVar: IntVar) =>
      (Gen.choose(rangeValue.min, rangeValue.max).sample.get, boundVar))
    SortedMap(map: _*)
  }

  /**
   * Method for generating a random IntSetVar to add to the bench and to its
   * model.
   */
  def genIntSetVar(
    nbVars: Int = 5,
    range: Range = 0 to 100,
    isInput: Boolean = true) = {
    val risVar = InvGen.randomFixedIntSetVar(nbVars, range, model).sample.get
    addVar(isInput, risVar)
    risVar.randomVar
  }

  /**
   * Method for generating an array of random IntSetVar to add to the bench
   * and to its model.
   */
  def genIntSetVars(
    nbVars: Int = 4,
    upToSize: Int = 20,
    range: Range = 0 to 100,
    isInput: Boolean = true): Array[IntSetVar] = {
    val risVars = InvGen.randomIntSetVars(nbVars, upToSize, range, model).sample.get
    addVar(isInput, risVars)
    risVars.map((risv: RandomIntSetVar) => {
      risv.randomVar
    }).toArray
  }

  /**
   * For debug only
   */
  def printVars(name: String, vars: List[RandomVar]) {
    if (vars.length > 0) {
      println(name + " vars:")
      vars.foreach((rv: RandomVar) => println(rv.toString()))
      println
    }
  }

  /**
   * This method runs the bench.
   */
  def run() = {
    model.close()
    //println("Model closed")
    model.propagate()

    try {
      property = org.scalacheck.Prop.forAll(InvGen.move) {
        randomMove: Move =>
          if (verbose > 0) {
            println("---------------------------------------------------")
            printVars("Input", inputVars)
            printVars("Output", outputVars)
            print(randomMove.toString() + " ")
          }
          val randomVar = Gen.oneOf(inputVars).sample.get
          if (verbose > 0) print(randomVar.toString() + " => ")
          randomVar.move(randomMove)
          if (verbose > 0) println(randomVar.toString() + "\n")
          model.propagate()
          if (verbose > 0) println
          checker.isChecked
      }
    } catch {
      case e: Exception =>
        println("Exception caught: " + e)
    }
    Checkers.check(property)
  }
}
