package oscar.cp.core

import oscar.algo.reversible.ReversibleQueue
import oscar.algo.reversible.ReversiblePointer
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.implementations.Requires
import oscar.cp.constraints.implementations.Excludes
import oscar.cp.constraints.implementations.SetCard

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPVarSet(val store: CPStore, min: Int, max: Int, val name: String = "") extends CPVar {

  private val dom = new SetDomain(store, min, max)

  val onDomainL2 = new ReversiblePointer[ConstraintQueue](store, null)

  val onRequiredL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onExcludedL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onRequiredIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)
  val onExcludedIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](store, null)

  // cardinality variable
  val card = CPVarInt(0, max - min + 1)(store)
  store.post(new SetCard(this, card))

  /**
   * @return true if the domain of the variable has exactly one value,
   * false if the domain has more than one value
   */
  def isBound: Boolean = dom.possibleSize == dom.requiredSize

  /**
   * Test if a value is in the possible values
   * @param val
   * @return  true if value is in the possible values false otherwise
   */
  def isPossible(value: Int) = dom.isPossible(value)

  /**
   * Test if a value is in the required values
   * @param val
   * @return  true if value is in the required values false otherwise
   */
  def isRequired(value: Int) = dom.isRequired(value)

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    onDomainL2.setValue(new ConstraintQueue(onDomainL2.value, c));
    if (trackDelta) c.addSnapshot(this)
  }

  def filterWhenDomainChanges(filter: DeltaVarSet => CPOutcome) {
    store.post(
      new DeltaVarSet(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenDomainChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredWhenRequiredValue(c: Constraint) {
    onRequiredL1.setValue(new PropagEventQueueVarSet(onRequiredL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedWhenExcludedValue(c: Constraint) {
    onExcludedL1.setValue(new PropagEventQueueVarSet(onExcludedL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredIdxWhenRequiredValue(c: Constraint, idx: Int) {
    onRequiredIdxL1.setValue(new PropagEventQueueVarSet(onRequiredIdxL1.value, c, this, idx))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedIdxWhenExcludedValue(c: Constraint, idx: Int) {
    onExcludedIdxL1.setValue(new PropagEventQueueVarSet(onExcludedIdxL1.value, c, this, idx))
  }

  def requires(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      store.notifyL2(onDomainL2.value)
      // -------- AC5 notifications ------------
      store.notifyRequired(onRequiredL1.value, this, v)
      store.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
    }
    val oc = dom.requires(v)
    if (oc != CPOutcome.Failure) {
      if (requiredSize == card.max) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = excludes(a)
          assert(r != CPOutcome.Failure)
        }
      }
      card.updateMin(requiredSize)
    } else oc
  }

  def excludes(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      store.notifyL2(onDomainL2.value)
      // -------- AC5 notifications ------------
      store.notifyExcluded(onExcludedL1.value, this, v)
      store.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
    }
    val oc = dom.excludes(v)
    if (oc != CPOutcome.Failure) {
      if (possibleSize == card.min) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = requires(a)
          assert(r != CPOutcome.Failure)
        }
      }
      card.updateMax(possibleSize)
    } else oc
  }

  def requiresAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) store.notifyL2(onDomainL2.value)
    // -------- AC5 notifications ------------
    if (onRequiredL1.hasValue() || onRequiredIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onRequiredL1.hasValue()) store.notifyRequired(onRequiredL1.value, this, v)
        if (onRequiredIdxL1.hasValue) store.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
      }
    }
    val oc = dom.requiresAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }

  def excludesAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) store.notifyL2(onDomainL2.value)
    // -------- AC5 notifications ------------
    if (onExcludedL1.hasValue() || onExcludedIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onExcludedL1.hasValue()) store.notifyExcluded(onExcludedL1.value, this, v)
        if (onExcludedIdxL1.hasValue) store.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
      }
    }
    val oc = dom.excludesAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }

  def arbitraryPossibleNotRequired: Int = dom.arbitraryPossibleNotRequired

  def randomPossibleNotRequired: Int = dom.randomPossibleNotRequired

  def value(): Set[Int] = dom.requiredSet

  def requiredSet(): Set[Int] = dom.requiredSet

  def possibleSet(): Set[Int] = dom.possibleSet

  def possibleNotRequiredValues: Iterator[Int] = dom.possibleNotRequiredValues

  def requiredValues: Iterator[Int] = dom.requiredValues

  def possibleSize: Int = dom.possibleSize

  def requiredSize: Int = dom.requiredSize

  def ++(v: Int): Constraint = new Requires(this, v)
  def --(v: Int): Constraint = new Excludes(this, v)

  override def toString: String = {
    val required = requiredValues.toSet
    val possible = possibleNotRequiredValues.toSet
    s"$required $possible"
  }

  // ------ delta methods to be called in propagate -------

  def changed(sn: SnapshotVarSet): Boolean = possibleChanged(sn) || requiredChanged(sn)

  def possibleChanged(sn: SnapshotVarSet): Boolean = sn.oldSizePossible != possibleSize

  def requiredChanged(sn: SnapshotVarSet): Boolean = sn.oldSizeRequired != requiredSize

  def deltaPossibleSize(sn: SnapshotVarSet): Int = sn.oldSizePossible - possibleSize

  def deltaRequiredSize(sn: SnapshotVarSet): Int = requiredSize - sn.oldSizeRequired

  def deltaPossible(sn: SnapshotVarSet): Iterator[Int] = dom.deltaPossible(sn.oldSizePossible)

  def deltaRequired(sn: SnapshotVarSet): Iterator[Int] = dom.deltaRequired(sn.oldSizeRequired)

  def ==(y: CPVarSet): Constraint = new oscar.cp.constraints.implementations.SetEq(this, y)
}

object CPVarSet {
  
  /** Creates a new CP Var Set that can contain a set of possible values in which some are required */
  def apply(possible: Set[Int], required: Set[Int])(implicit store: CPStore): CPVarSet = {
    if (!required.forall(elem => possible.contains(elem))) {
      throw new RuntimeException("Required values should be possible")
    }
    val set = new CPVarSet(store, possible.min, possible.max) 
    // Initializes the possible element
    for (elem <- possible.min to possible.max if !possible.contains(elem)) {
      set.excludes(elem)
    }
    // Initializes the required elements
    required.foreach(set.requires)
    set
  }
  
  /** Creates a new CP Var Set that can contain a set of possible values */
  def apply(possible: Set[Int])(implicit store: CPStore): CPVarSet = apply(possible, Set())(store)
  
  @deprecated("use apply(required: Set[Int], possibleNotRequired: Set[Int])(implicit store: CPStore) instead", "1.0")
  def apply(s: CPStore, required: Set[Int] = Set(), possibleNotRequired: Set[Int]): CPVarSet = {
    if (!required.intersect(possibleNotRequired).isEmpty) {
      throw new RuntimeException("Possible values should not be required")
    }
    val allPossibles = required ++ possibleNotRequired
    val x = new CPVarSet(s, allPossibles.min, allPossibles.max)
    for (v <- allPossibles.min to allPossibles.max if !allPossibles.contains(v)) {
      x.excludes(v)
    }
    for (v <- required) {
      x.requires(v)
    }
    x
  }
}
  
