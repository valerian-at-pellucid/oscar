package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar, Store}
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.search.{StopWatch, SearchEngineTrait}
import oscar.cbls.modeling._
import oscar.cbls.constraints.core.{ConstraintSystem, Constraint}
import oscar.cbls.objective.ObjectiveTrait
import scala.collection.immutable.SortedSet


/** this is a helper object that you can extend to implement your solver with the minimal syntactic overhead.
  * It imports all the methods that you might need to develop your own solver based on the CBLS approach
  *
  * @param verbose requires that the propagation structure prints a trace of what it is doing. all prints are preceded by ''PropagationStruture''
  * @param checker specifies that once propagation is finished, it must call the checkInternals method on all propagation elements.
  * @param noCycle is to be set to true only if the static dependency graph between propagation elements has no cycles. If unsure, set to false, the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
  * @param topologicalSort set to true if you want to use topological sort, to false for layered sort (layered is faster)
  * @param propagateOnToString set to true if a toString triggers a propagation, to false otherwise. Set to false only for deep debugging
  *
  **/
class CBLSModel(val verbose:Boolean = false,
                 val checker:Option[Checker] = None,
                 val noCycle:Boolean = true,
                 val topologicalSort:Boolean = false,
                 val propagateOnToString:Boolean = true)
  extends SearchEngineTrait
  with AlgebraTrait
  with Constraints
  with ClusterInvariants
  with ComplexLogicInvariants
  with AccessInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with StopWatch{

  implicit val s = new Store(verbose, checker, noCycle, topologicalSort,propagateOnToString)
  implicit val c = new ConstraintSystem(s)

  def intVar(r:Range, v:Int, name:String = "")(implicit s:Store) = new CBLSIntVar(s,r,v,name)
  def setVar(r:Range, v:Iterable[Int], name:String="")(implicit s:Store) = {
    val emptySet:SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, r.start, r.end,name, emptySet ++ v)
  }

  def close()(implicit s:Store) {s.close()}

  def add(c:Constraint)(implicit cs:ConstraintSystem) {cs.post(c)}
  def post(c:Constraint)(implicit cs:ConstraintSystem) {cs.post(c)}

  def violation()(implicit cs:ConstraintSystem) = cs.violation
  def solution()(implicit s:Store) = s.solution()

  def swapVal(a:CBLSIntVar, b:CBLSIntVar)(implicit o:ObjectiveTrait) = o.swapVal(a,b)
  def assignVal(a: CBLSIntVar, v: Int)(implicit o:ObjectiveTrait) = o.assignVal(a, v)

}
