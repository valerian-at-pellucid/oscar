package oscar.flatzinc.cbls

import scala.collection.immutable.SortedSet
import scala.collection.mutable.{ Map => MMap }
import oscar.cbls.search._
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.core.{Constraint => CBLSConstraint}
import oscar.cbls.invariants.core.computation.{Variable => CBLSVariable}
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.constraints.lib.global._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
import oscar.cbls.invariants.core.computation.CBLSIntVar.int2IntVar
import oscar.flatzinc.model._
import oscar.flatzinc.model.Variable
import oscar.flatzinc.model.Constraint
import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.SortedMap
import oscar.flatzinc.NoSuchConstraintException
import oscar.cbls.modeling.Invariants
import oscar.cbls.invariants.lib.logic.Cumulative
import scala.collection.immutable.TreeSet
import oscar.flatzinc.cbls.support.CBLSIntVarDom
import oscar.flatzinc.cbls.support.CBLSIntConstDom

trait FZCBLSConstraints {
  implicit val m: Store = new Store(false, None, false)//setting the last Boolean to true would avoid calling the SCC algorithm but we have to make sure that there are no SCCs in the Graph. Is it the case in the way we build it?
  
  //TODO: Not really tested
  def Weight(c: CBLSConstraint,w:Int):CBLSConstraint = {
    EQ(0,Prod2(c.violation,CBLSIntConst(w,m)))
  }
  /*
  class Weight(c: CBLSConstraint,w:Int) extends CBLSConstraint {
    val wx = CBLSIntConst(w,m)
    def violation:CBLSIntVar = Prod2(c.violation,wx).toIntVar
    def violation(v: CBLSVariable): CBLSIntVar = Prod2(c.violation(v),wx).toIntVar
    override def constrainedVariables = c.constrainedVariables
    override def registerConstrainedVariable(v: CBLSVariable){
      c.registerConstrainedVariable(v)
    } 
    override implicit def toIntVar:CBLSIntVar = c.toIntVar
    override def setOutputVar(v:CBLSIntVar) = c.setOutputVar(v)
  }*/
  class ValueTracker(v: CBLSIntVarDom)(implicit c: ConstraintSystem) {
    val initialMin = v.minVal;
    val initialMax = v.maxVal;
    val weight = CBLSIntConst(10,m);//why 10?
    def update(force: Boolean  = false) = {
      //TODO: Maybe we should post this in all cases because not all invariants properly modify the domain of the output var.
      //Todo:Take into account non-range domains.
      if (force || v.minVal < initialMin) {
       // println("%% Needed to constrain the output domain of an invariant");
       // println(v + " " +v.getDomain() + " " + v.domain + " m " + initialMin)
        c.add(GE(v, initialMin),weight)
      }
      if (force || v.maxVal > initialMax) {
       // println("%% Needed to constrain the output domain of an invariant");
       // println(v + " " +v.getDomain()+ " " + v.domain + " M " + initialMax+" " + v.maxVal)
        c.add(LE(v, initialMax),weight)
      }
    }
  }
  object ValueTracker {
    def apply(v: CBLSIntVarDom)(implicit c: ConstraintSystem) = {
      new ValueTracker(v)
    }
  }
  object EnsureDomain{
     def apply(v: CBLSIntVarDom)(implicit c: ConstraintSystem) = {
      val t = new ValueTracker(v)
      t.update(true)
    }
  }
  object InvariantEnsureDomain{
    def apply(v: CBLSIntVarDom, i:IntInvariant)(implicit c: ConstraintSystem) = {
      val t = new ValueTracker(v)
      v <== i
      t.update()
    }
  }
  
  implicit def getCBLSVar(v: Variable)(implicit cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    v match {
      case ConcreteConstant(_, value, _) =>
        //All constants need to have a store, otherwise they won't have a UniqueID (from PropagationElement) and constraints will start throwing exceptions
        cblsIntMap.get(value + "") match {
          case None =>
            val c = CBLSIntConstDom(value, m);
            cblsIntMap += value + "" -> c;
            c;
          case Some(c) => c;
        }

      case ConcreteVariable(id, _, _) =>
        cblsIntMap.get(id).get;
    }
  }
  /*
  implicit def getVar(v: String)(implicit cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    cblsIntMap.get(v).get;
  }*/

  
  
  /*def get_count_eq(xs:Array[Variable], y: Variable, cnt:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //xs domain goes from i to j but cnts will be from 0 to i-j, so need to use the offset (built by DenseCount)
    val dc = DenseCount.makeDenseCount(xs.map(getCBLSVar(_)));
    val cnts = dc.counts
    EQ(cnt,IntElement(y,cnts,dc.offset))
  }*/
  
  
  def get_alldifferent(xs: Array[Variable], ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    AllDiff(xs.map(getCBLSVar(_)))
  }
  def get_cumulative(s: Array[Variable], d: Array[Variable],r: Array[Variable],b: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val indices = s.indices.toArray[Int]
    //assumes the planning starts at zero!
    val start = s.foldLeft(Int.MaxValue)((acc,v) => if (v.minVal < acc) v.minVal else acc)
    val horizon = s.foldLeft(Int.MinValue)((acc,v) => if (v.maxVal > acc) v.maxVal else acc)
    val p = new Array[CBLSIntVar](horizon-start+1)
    val a = new Array[CBLSSetVar](horizon-start+1)
    val ns = new Array[CBLSIntVar](s.length)
    for(i <- 0 to horizon-start){
      p(i) = CBLSIntVar(c.model,0 to b.max,0,"Profile("+i+")")
      a(i) = new CBLSSetVar(c.model,0,s.length-1,"Active("+i+")")
    }
    val offset = new CBLSIntConst(-start,c.model)
    for(i <- 0 to s.length-1){
      ns(i) = CBLSIntVar(c.model,0, horizon-start,0,"OffsetStart("+i+")")
      ns(i) <== Sum2(s(i),offset)
    }
    //println(offset)
    //println(ns.map(v=>v.minVal).mkString(","))
    //println(ns.map(v=>v.value).mkString(","))
    val cumul = Cumulative(indices,ns,d.map(getCBLSVar(_)),r.map(getCBLSVar(_)),p,a);
    /*for(i <- 0 to horizon-start){
      c.add(GE(b,p(i)));
    }*/
    GE(b,MaxArray(p));
  }
  
  
  
  def get_array_bool_and_inv(as: Array[Variable], r: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    EQ(Prod(as.map(getCBLSVar(_))), 1)
  }
  
  def get_array_int_element_inv(b: Variable, as: Array[Variable], r: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    IntElement(Sum2(b,-1), as.map(getCBLSVar(_)))
  }
  
  def get_array_bool_or_inv(as: Array[Variable], r: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    GE(Sum(as.map(getCBLSVar(_))), 1)
  }
  def get_array_bool_xor(as: Array[Variable], ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    EQ(Mod(Sum(as.map(getCBLSVar(_))), 2), 1)
  }
  def get_array_bool_xor_inv(as: Array[Variable], defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val index = as.indexWhere(p => p.id == defId);
    val defVar = as(index);
    val vars2 = (as.take(index) ++ as.drop(index + 1)).map(getCBLSVar(_));
    EQ(Mod(Sum2(Sum(vars2), 1), 2), 1)
  }

  def get_bool_clause(as: Array[Variable], bs: Array[Variable], ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: This can also be expressed with the element constraint, maybe that is faster?
    NE(Sum2(GE(Sum(as.map(getCBLSVar(_))), 1), EQ(Prod(bs.map(getCBLSVar(_))), 0)), 0)
  }

  def get_bool_not_inv(a: Variable, b: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    if (a.id == defId) {
      EQ(b, 0)
    } else {
      EQ(a, 0)
    }
  }

  def get_bool_or_inv(a: Variable, b: Variable, r: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    G(Sum2(a, b), 0)
  }

  
  def get_int_abs_inv(a: Variable, b: Variable, defId: String, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    Abs(a)
  }

  
  def get_int_div_inv(a: Variable, b: Variable, c: Variable, defId: String, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: can this also define a and b? NO
    Div(a, b)
  }

  def get_int_eq_inv(x: Variable, y: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    if (x.id == defId) {
      y.getClone
    } else {
      x.getClone
    }
  }


  def get_int_le(x: Variable, y: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    LE(x, y)
  }

  def get_int_lin_eq(params: Array[Variable], vars: Array[Variable], sum: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val prodArray = Array.tabulate(vars.length)(n => Prod2(params(n), vars(n)).toIntVar);
    EQ(Sum(prodArray).toIntVar, sum)
  }
  
  //TODO: Why is params an array of _Variable_ and not _Parameters_?
  def get_int_lin_eq_inv(params: Array[Variable], vars: Array[Variable], sum: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val index = vars.indexWhere(p => p.id == defId);
    val defParam = params(index);
    val defVar = vars(index);
    val params2 = params.take(index) ++ params.drop(index + 1)
    val vars2 = vars.take(index) ++ vars.drop(index + 1)
    val prodArray = Array.tabulate(vars2.length)(n => Prod2(params2(n), vars2(n)).toIntVar);
    if (defParam.min == 1) {
     // println("%post "+sum+ " - sum("+prodArray.mkString(", ")+")")
      Minus(sum, Sum(prodArray))
    } else if (defParam.min == -1) {
     // println("%post - "+sum+ " + sum("+prodArray.mkString(", ")+")")
      Minus(Sum(prodArray), sum)
    } else {
      println("% Defining var with a scalar that isn't +-1, this can cause serious problems")
      Div(Minus(sum, Sum(prodArray)), defParam)
    }
  }

  def get_int_lin_le(params: Array[Variable], vars: Array[Variable], sum: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val prodArray = Array.tabulate(vars.length)(n => Prod2(params(n), vars(n)).toIntVar);
    LE(Sum(prodArray).toIntVar, sum)
  }

  def get_int_lin_ne(params: Array[Variable], vars: Array[Variable], sum: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: can this define vars? NO
    val prodArray = Array.tabulate(vars.length)(n => Prod2(params(n), vars(n)).toIntVar);
    NE(Sum(prodArray).toIntVar, sum)
  }

  def get_int_lt(a: Variable, b: Variable, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    L(a, b)
  }

  def get_int_max(a: Variable, b: Variable, c: Variable, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    EQ(Max2(a, b), c)
  }
  def get_int_max_inv(a: Variable, b: Variable, c: Variable, defId: String, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    if (defId == c.id) {
      Max2(a, b)
    } else if (defId == a.id) { // This is superweird but it happened in a model...
      Max2(c, b)
    } else {
      Max2(a, c)
    }
  }

  def get_int_min(a: Variable, b: Variable, c: Variable, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    EQ(Min2(a, b), c)
  }
  def get_int_min_inv(a: Variable, b: Variable, c: Variable, defId: String, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    if (defId == c.id) {
      Min2(a, b)
    } else if (defId == a.id) { // This is superweird but it happened in a model...
      Min2(c, b)
    } else {
      Min2(a, c)
    }
  }

  def get_int_mod_inv(a: Variable, b: Variable, c: Variable, defId: String, ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    Mod(a, b)
  }

  def get_int_ne(x: Variable, y: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    NE(x, y)
  }

  def get_int_plus(x: Variable, y: Variable, z: Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    EQ(Sum2(x, y), z)
  }
  def get_int_plus_inv(x: Variable, y: Variable, z: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    if (x.id == defId) {
      Minus(z, y)
    } else if (y.id == defId) {
      Minus(z, x)
    } else {
      Sum2(x, y)
    }
  }

  def get_int_times_inv(x: Variable, y: Variable, z: Variable, defId: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: Can times define x and y? NO
    assert(defId.equals(z.id));
    Prod2(x, y)
  }
  
  def get_set_in(x: Variable, s: Domain, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    var sset = s match {
      case DomainRange(mi,ma) => SortedSet[Int]() ++ (mi to ma)
      case DomainSet(vals) => SortedSet[Int]() ++ vals
    }
    val setVar = new CBLSSetConst(sset,c._model)
    BelongsTo(x, setVar)
  }
  
  def get_maximum_inv(x: Array[Variable], ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    MaxArray(x.map(getCBLSVar(_)))
  }
  def get_minimum_inv(x: Array[Variable], ann: List[Annotation])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    MinArray(x.map(getCBLSVar(_)))
  }
  
  def get_inverse(xs: Array[Variable], ys:Array[Variable])(implicit cs: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: Add alldiff as redundant constraint?
    //TODO: check the index_sets? Assumes it starts at 1
    xs.zipWithIndex.map{case (xi,i) => EQ(i,Sum2(IntElement(Sum2(xi,-1),ys.map(getCBLSVar(_))),-1))}.toList
  }
  
  def get_count_eq_inv(xs:Array[Variable], y: Variable, cnt:Variable, defined: String, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //TODO: DenseCount might be quite expensive...
    //xs domain goes from i to j but cnts will be from 0 to i-j
    val dc = DenseCount.makeDenseCount(xs.map(getCBLSVar(_)));
    val cnts = dc.counts
    IntElement(Sum2(y,dc.offset),cnts);
  }
  
  def get_at_least_int(n:Variable,xs: Array[Variable], v:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    AtLeast(xs.map(getCBLSVar(_)),SortedMap((v.min,n)));
  }
  def get_at_most_int(n:Variable,xs: Array[Variable], v:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    AtMost(xs.map(getCBLSVar(_)),SortedMap((v.min,n.min)));
  }
  def get_exactly_int(n:Variable,xs: Array[Variable], v:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    List(AtMost(xs.map(getCBLSVar(_)),SortedMap((v.min,n.min))),AtLeast(xs.map(getCBLSVar(_)),SortedMap((v.min,n))));
  }
  /*def get_among_inv(n:Variable,xs: Array[Variable], v:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    List(AtMost(xs.map(getCBLSVar(_)),SortedMap((v.min,n.min))),AtLeast(xs.map(getCBLSVar(_)),SortedMap((v.min,n))));
  }*/
  //constrains all variables in xs to take their value in dom
  def domains(xs: Array[Variable], dom: Array[Int])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val setVar = new CBLSSetConst(dom.to[SortedSet],c._model)
    xs.toList.map(x => Weight(BelongsTo(getCBLSVar(x),setVar),100))
  }
  def get_global_cardinality_low_up(closed: Boolean, xs: Array[Variable],vs: Array[Variable],lows: Array[Int],ups:Array[Int])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    val atleast = AtLeast(xs.map(getCBLSVar(_)),SortedMap(vs.zip(lows).map(vl => (vl._1.min,new CBLSIntConst(vl._2,m))): _*))
    val atmost = AtMost(xs.map(getCBLSVar(_)),SortedMap(vs.zip(ups).map(vl => (vl._1.min,vl._2)): _*))
    List(atleast,atmost) ++ (if(closed) domains(xs,vs.map(_.min)) else List())
  }
  def get_global_cardinality(closed: Boolean, xs: Array[Variable],vs: Array[Variable],cnts: Array[Variable])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
     if(cnts.forall(c => c.min==c.max)){//fixed counts
       get_global_cardinality_low_up(closed,xs,vs,cnts.map(_.min),cnts.map(_.max))
     }else{
       //TODO: Might be more efficient...
       val dc = DenseCount.makeDenseCount(xs.map(getCBLSVar(_)));
       val counts = dc.counts
       val eqs = vs.toList.zip(cnts).map(_ match {case (v,c) => EQ(c,counts(v.min-dc.offset))})//TODO: +offset or -offset?
       if(closed) domains(xs,vs.map(_.min)) ++ eqs else eqs
     }
  }
  implicit def cstrListToCstr(cstrs: List[CBLSConstraint]): CBLSConstraint = {
    val cs = new ConstraintSystem(m)
    for(cstr <- cstrs){
      cs.add(cstr)
    }
    cs
  }
  
  def constructCBLSConstraint(constraint: Constraint)(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]):CBLSConstraint = {
    constraint match {
      case reif(cstr,r) => EQ(r,constructCBLSConstraint(cstr))
      
      case array_bool_and(as, r, ann)                 => EQ(r,get_array_bool_and_inv(as, r,r.id, ann))
      case array_bool_element(b, as, r, ann)          => EQ(r,get_array_int_element_inv(b, as, r, r.id, ann))
      case array_bool_or(as, r, ann)                  => EQ(r,get_array_bool_or_inv(as, r,r.id, ann))
      case array_bool_xor(as, ann)                    => get_array_bool_xor(as, ann)
      case array_int_element(b, as, r, ann)           => EQ(r,get_array_int_element_inv(b, as, r,r.id, ann))
      case array_var_bool_element(b, as, r, ann)      => EQ(r,get_array_int_element_inv(b, as, r,r.id, ann))
      case array_var_int_element(b, as, r, ann)       => EQ(r,get_array_int_element_inv(b, as, r,r.id, ann))

      case bool2int(x, y, ann)                        => EQ(x,get_int_eq_inv(x, y,x.id, ann))
      case bool_and(a, b, r, ann)                     => EQ(r,get_int_times_inv(a, b, r,r.id, ann))
      case bool_clause(a, b, ann)                     => get_bool_clause(a, b, ann)
      case bool_eq(a, b, ann)                         => EQ(a,get_int_eq_inv(a, b,a.id, ann))
      case bool_le(a, b, ann)                         => get_int_le(a, b, ann)
      case bool_lin_eq(params, vars, sum, ann)        => get_int_lin_eq(params, vars, sum, ann)
      case bool_lin_le(params, vars, sum, ann)        => get_int_lin_le(params, vars, sum, ann)
      case bool_lt(a, b, ann)                         => get_int_lt(a, b, ann)
      case bool_not(a, b, ann)                        => get_int_ne(a, b, ann)
      case bool_or(a, b, r, ann)                      => EQ(r,get_bool_or_inv(a, b, r, r.id, ann))
      case bool_xor(a, b, r, ann)                     => EQ(r,get_int_ne(a, b, ann))

      case int_abs(x, y, ann)                         => EQ(y,get_int_abs_inv(x, y, y.id,ann))
      case int_div(x, y, z, ann)                      => EQ(z,get_int_div_inv(x, y, z,z.id, ann))
      case int_eq(x, y, ann)                          => EQ(x,get_int_eq_inv(x, y,x.id, ann))
      case int_le(x, y, ann)                          => get_int_le(x, y, ann)
      case int_lin_eq(params, vars, sum, ann)         => get_int_lin_eq(params, vars, sum, ann)
      case int_lin_le(params, vars, sum, ann)         => get_int_lin_le(params, vars, sum, ann)
      case int_lin_ne(params, vars, sum, ann)         => get_int_lin_ne(params, vars, sum, ann)
      case int_lt(x, y, ann)                          => get_int_lt(x, y, ann)
      case int_max(x, y, z, ann)                      => get_int_max(x, y, z, ann)
      case int_min(x, y, z, ann)                      => get_int_min(x, y, z, ann)
      case int_mod(x, y, z, ann)                      => EQ(z,get_int_mod_inv(x, y, z,z.id, ann))
      case int_ne(x, y, ann)                          => get_int_ne(x, y, ann)
      case int_plus(x, y, z, ann)                     => get_int_plus(x, y, z, ann)
      case int_times(x, y, z, ann)                    => EQ(z,get_int_times_inv(x, y, z,z.id, ann))
      case set_in(x, s, ann)                          => get_set_in(x, s, ann)
      
      case all_different_int(xs, ann)                 => get_alldifferent(xs, ann)
      case at_least_int(n,xs,v,ann)                   => get_at_least_int(n,xs,v,ann)
      case at_most_int(n,xs,v,ann)                    => get_at_most_int(n,xs,v,ann)
      case cumulative(s,d,r,b,ann)                    => get_cumulative(s,d,r,b,ann)
      case count_eq(xs,y,cnt,ann)                     => EQ(cnt,get_count_eq_inv(xs,y,cnt,cnt.id,ann))
      case exactly_int(n,xs,v,ann)                    => get_exactly_int(n,xs,v,ann)
      case inverse(xs,ys,ann)                         => get_inverse(xs,ys)
      case global_cardinality_closed(xs,vs,cs,ann)    => get_global_cardinality(true,xs,vs,cs)
      case global_cardinality(xs,vs,cs,ann)           => get_global_cardinality(false,xs,vs,cs)
      case global_cardinality_low_up_closed(xs,vs,ls,us,ann) => get_global_cardinality_low_up(true,xs,vs,ls.map(_.min),us.map(_.min))
      case global_cardinality_low_up(xs,vs,ls,us,ann) => get_global_cardinality_low_up(false,xs,vs,ls.map(_.min),us.map(_.min))
      case maximum_int(y,xs,ann)                      => EQ(y,get_maximum_inv(xs,ann))
     // case member_int(xs,y,ann)                       => get_member(xs,y) use the decomposition
      case minimum_int(y,xs,ann)                      => EQ(y,get_minimum_inv(xs,ann))
      case notimplemented                             => throw new NoSuchConstraintException(notimplemented.toString(),"CBLS Solver");
    }
  }
  def constructCBLSIntInvariant(constraint: Constraint,id:String)(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]): IntInvariant = {
    constraint match {
      case reif(cstr,r) => constructCBLSConstraint(cstr)//.asInstanceOf[Invariant]
      
      case array_bool_and(as, r, ann)                 => get_array_bool_and_inv(as, r, id, ann)
      case array_bool_element(b, as, r, ann)          => get_array_int_element_inv(b, as, r, id, ann)
      case array_bool_or(as, r, ann)                  => get_array_bool_or_inv(as, r, id, ann)
      case array_bool_xor(as, ann)                    => get_array_bool_xor_inv(as, id, ann)
      case array_int_element(b, as, r, ann)           => get_array_int_element_inv(b, as, r, id, ann)
      case array_var_bool_element(b, as, r, ann)      => get_array_int_element_inv(b, as, r, id, ann)
      case array_var_int_element(b, as, r, ann)       => get_array_int_element_inv(b, as, r, id, ann)

      case bool2int(x, y, ann)                        => get_int_eq_inv(x, y, id, ann)
      case bool_and(a, b, r, ann)                     => get_int_times_inv(a, b, r, id, ann)
      case bool_eq(a, b, ann)                         => get_int_eq_inv(a, b, id, ann)
      case bool_lin_eq(params, vars, sum, ann)        => get_int_lin_eq_inv(params, vars, sum, id, ann)
      case bool_not(a, b, ann)                        => get_bool_not_inv(a, b, id, ann)
      case bool_or(a, b, r, ann)                      => get_bool_or_inv(a, b, r, id, ann)
      case bool_xor(a, b, r, ann)                     => get_int_ne(a, b, ann)//This assumes that only r can be defined!

      case int_abs(x, y, ann)                         => get_int_abs_inv(x, y, id, ann)
      case int_div(x, y, z, ann)                      => get_int_div_inv(x, y, z, id, ann)
      case int_eq(x, y, ann)                          => get_int_eq_inv(x, y, id, ann)
      case int_lin_eq(params, vars, sum, ann)         => get_int_lin_eq_inv(params, vars, sum, id, ann)
      case int_max(x, y, z, ann)                      => get_int_max_inv(x, y, z, id, ann)
      case int_min(x, y, z, ann)                      => get_int_min_inv(x, y, z, id, ann)
      case int_mod(x, y, z, ann)                      => get_int_mod_inv(x, y, z, id, ann)
      case int_plus(x, y, z, ann)                     => get_int_plus_inv(x, y, z, id, ann)
      case int_times(x, y, z, ann)                    => get_int_times_inv(x, y, z, id, ann)
     
      case count_eq(xs,y,cnt,ann)                     => get_count_eq_inv(xs,y,cnt,id,ann)
      case maximum_int(y,xs,ann)                      => get_maximum_inv(xs,ann)//assumes that the id is y.
      case minimum_int(y,xs,ann)                      => get_minimum_inv(xs,ann)
      
      case notimplemented                             => throw new NoSuchConstraintException(notimplemented.toString(),"CBLS Solver");
    }
  }
  def add_constraint(constraint: Constraint)(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    constraint.definedVar match {
      case None =>
        c.add(constructCBLSConstraint(constraint))
      case Some(v) =>
        val id = v.id
        InvariantEnsureDomain(v,constructCBLSIntInvariant(constraint,id))
    }
  }
}