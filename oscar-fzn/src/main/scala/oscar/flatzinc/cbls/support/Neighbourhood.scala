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
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls.support
import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.objective.{ Objective => CBLSObjective }
import oscar.cbls.invariants.lib.logic.Cluster
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.objective.{Objective => CBLSObjective}
import oscar.cbls.search.SearchEngine
import scala.collection.mutable.{Map => MMap}




abstract class Move(val value:Int){
  def commit():Unit
  def getModified: Set[CBLSIntVar]
}
class AssignMove(x: CBLSIntVar,k:Int,value:Int) extends Move(value){
  def commit(){x := k}
  def getModified=Set(x)
  override def toString() = x + " assigned to " +k
}
class AssignsMove(xk: List[(CBLSIntVar,Int)],value:Int) extends Move(value){
  def commit(){xk.foreach(x => x._1 := x._2)}
  def getModified = xk.map(_._1).toSet
  override def toString() = xk.foldLeft("")((acc,x) => acc+x._1+ " assigned to " +x._2 +"; ")
}
class SwapMove(x: CBLSIntVar,y:CBLSIntVar,value:Int) extends Move(value){
  def commit(){x :=: y}
  def getModified=Set(x,y)
  override def toString() = x + " swapped with " +y
}
class NoMove(value:Int = Int.MaxValue) extends Move(value){
  def commit(){}
  def getModified = Set.empty[CBLSIntVar]
  override def toString() = "No-Op"
}
class BeforeMove(m: Move,act:()=>Unit) extends Move(m.value) {
  def commit(){
    act()
    m.commit()
  }
  def getModified = m.getModified
  override def toString() = m.toString()
}






//Extends SearchEngine to access the selectors
abstract class Neighbourhood(val searchVariables: Array[CBLSIntVarDom]) extends SearchEngine {
  //var minObjective: Int = Int.MaxValue;
  def getVariables(): Array[CBLSIntVarDom] = searchVariables
  
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move;
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move;
  def randomMove(it: Int): Move;
  
 // def init(): Unit;
  def reset(): Unit;
  def violation(): Int;//TODO used in only one place. Useful?
}

//assumes that all variables have a range domain!
class SumNeighborhood(val variables: Array[CBLSIntVarDom],val coeffs:Array[Int], val sum:Int, objective: CBLSObjective, cs: ConstraintSystem) extends Neighbourhood(variables){
  val variableViolation: Array[CBLSIntVar] = variables.map(cs.violation(_)).toArray
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
  
  reset();
  def reset(){
    var s = 0
    val vals = new Array[Int](variables.length)
    for(i <- 0 until variables.length){
      vals(i) = variables(i).minVal  
      s += coeffs(i) * vals(i)
    }
    //TODO: Improve this stupid code!
    while(s != sum){
      val i = RandomGenerator.nextInt(variables.length)
      if(s < sum && coeffs(i) > 0 && vals(i) < variables(i).maxVal){
        vals(i) += 1
        s += coeffs(i) * 1
      }else if(s > sum && coeffs(i) < 0 && vals(i) < variables(i).maxVal){
        vals(i) += 1
        s += coeffs(i) * 1
      }
    }
    for(i <- 0 until variables.length){
      if(variables(i).minVal > vals(i) || variables(i).maxVal < vals(i))throw new Exception("Problem")
      variables(i) := vals(i)
    }
  }
  
  def getMove(idx1:Int,diff:Int,idx2:Int): Move = {
    val mv = List((variables(idx1),variables(idx1).value+diff),(variables(idx2),variables(idx2).value-coeffs(idx1)*coeffs(idx2)*diff))
    new AssignsMove(mv,objective.assignVal(mv))
  }
  
  def randomMove(it:Int):Move = {
    new NoMove()
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    getExtendedMinObjective(it,nonTabu)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    val part1 = selectMinImb(rng,(i:Int) => variables(i).minVal to variables(i).maxVal,(i:Int,v:Int) => objective.assignVal(variables(i),v))
    part1 match{ 
      case (i1,v1) => 
        val diff = v1 - variables(i1).value
        val part2 = selectMin(List(0),rng)((k:Int,i2:Int) => getMove(i1,diff,i2).value, (k:Int,i2:Int) => {val nv = variables(i2).value-coeffs(i1)*coeffs(i2)*diff; i1!=i2 && nv >= variables(i2).minVal && nv <= variables(i2).maxVal})
        part2 match{
          case (0,i2) =>
	        if(diff==0) new NoMove()
	        else getMove(i1,diff,i2)
          case _ => new NoMove()
        }
      case _ => new NoMove()
    }
  }
  
  def selectMinImb[R,S](r: Iterable[R] , s: R => Iterable[S],f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true), stop: (R,S,Int) => Boolean = (_:R,_:S,_:Int) => false): (R,S) = {
	val flattened:List[(R,S)] = for (rr <- r.toList; ss <- s(rr).toList) yield (rr,ss)
	selectMin[(R,S)](flattened)((rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => st(rands._1,rands._2))
  }
}



class GCCNeighborhood(val variables: Array[CBLSIntVarDom],val vals:Array[Int],val low:Array[Int],val up:Array[Int], val closed:Boolean, objective: CBLSObjective, cs: ConstraintSystem)extends Neighbourhood(variables){
  val variableViolation: Array[CBLSIntVar] = variables.map(cs.violation(_)).toArray

  val counts = vals.foldLeft(Map.empty[Int,CBLSIntVar])((map,ic) => map + (ic -> new CBLSIntVar(cs._model,0 to variables.length,0,"count"+ic)))
  Cluster.MakeSparse(variables.map(c => c), vals).Clusters.foreach(ic => counts(ic._1) <== Cardinality(ic._2))
  //foldLeft(Map.empty[Int,CBLSIntVar])((map,ic) => map + (ic._1 -> Cardinality(ic._2).output))
  val lows = vals.toList.zip(low).foldLeft(Map.empty[Int,Int])((map,vl) => map + (vl._1 -> vl._2))
  val ups = vals.toList.zip(up).foldLeft(Map.empty[Int,Int])((map,vl) => map + (vl._1 -> vl._2))
  val alldoms = variables.foldLeft((Int.MaxValue,Int.MinValue))((set,v) => (math.min(set._1,v.dom.min),math.max(set._2,v.dom.max)))
  reset();
  //TODO: reset() should only be called after the model is closed, in case it makes use of invariants!
  def reset() = {
    //TODO: This reset does not respect the domains of the variables!
    var cur = variables.map(_.value)
    var cnts = cur.foldLeft(MMap.empty[Int,Int])((map,v) => map + (v -> (map.getOrElse(v, 0) + 1)))
    def cand():List[Int] = {
      List.tabulate(cur.length)(i=>i).filter(i => cnts(cur(i)) > lows.getOrElse(cur(i),0))
    }
    var cands = cand()
    //reaching the lowerbounds
    for(v <- vals){
      while(cnts.getOrElse(v,0) < lows(v)){
        if(cands.isEmpty)throw new Exception("GCC cannot be satisfied")
        val curvar = cands.head
        cands = cands.tail
        if(variables(curvar).inDomain(v) && cnts(cur(curvar))>lows.getOrElse(cur(curvar),0)){
          cnts(cur(curvar)) = cnts(cur(curvar)) - 1
          cnts(v) = cnts.getOrElse(v, 0) + 1
          cur(curvar) = v
        }
      }
      cands = cand()
    }
    //reaching the upperbounds
    for(v <- vals){
      var cands = List.tabulate(cur.length)(i=>i).filter(i => cur(i) == v)
      while(cnts.getOrElse(v,0) > ups(v)){
        if(cands.isEmpty)throw new Exception("GCC cannot be satisfied")
        val curvar = cands.head
        cands = cands.tail
        val candval = variables(curvar).getDomain().find(k => if(ups.contains(k)) cur(k) < ups(v) else !closed )
        if(candval.isDefined){
          val k = candval.get 
          cnts(v) = cnts(v)-1
          cnts(k) = cnts.getOrElse(k,0) +1
          cur(curvar) = k 
        }
      }
    }
    //updating the variables
    for(i <- 0 until variables.length){
      if(variables(i).minVal > cur(i) || variables(i).maxVal < cur(i))throw new Exception("Problem")
      variables(i) := cur(i)
    }
    /*
    var v = vals(0)
    var i = 0;
    var untouched = Set.empty[Int]
    for(v <- 0 until vals.length){
      var c = 0;
      while(c < low(v)){
        if(variables(i).inDomain(vals(v))){
          variables(i) := vals(v)
          c += 1
        }else{
          untouched += i
        }
        i += 1
      }
    }
    for(v <- 0 until vals.length){
      var c = low(v)
      while(i < variables.length && c < up(v)){
        variables(i) := vals(v)
        c += 1
        i += 1
      }
    }
    //need to put some variables outside, can only happen if not closed 
    if(i < variables.length){
      if(closed){
        throw new Exception("Closed GCC cannot be satisfied")
      }
      //TODO I am not happy with this code.
      while(i < variables.length){
        var found = false
        var v = variables(i).minVal
        while(!found){
          if(vals.forall(_!=v)){
            variables(i) := v
            i += 1
            found = true
          }
        }
      }
    }*/
  }
  def getSwapMove(idx1:Int,idx2:Int): Move = {
    //Swap will always respect the constraint is it was already satisfied
    new SwapMove(variables(idx1),variables(idx2),objective.swapVal(variables(idx1),variables(idx2)))
  }
  def getAssignMove(idx1:Int,v:Int): Move = {
    val cur = variables(idx1).value
    val cnt = counts.get(cur) match {case None => 1 case Some(x) => x.value}
    val lb = lows.getOrElse(cur,0)
    if(cnt <= lb) return new NoMove(Int.MaxValue);//using <= to protect from potential errors?
    val cnt2 = counts.get(cur) match {case None => 0 case Some(x) => x.value}
    val ub = ups.getOrElse(cur,1)
    if(cnt2 >= ub) return new NoMove(Int.MaxValue);//using >= to protect from potential errors?
    return new AssignMove(variables(idx1),v,objective.assignVal(variables(idx1), v));
  }
  
  def randomMove(it:Int):Move = {
    //TODO: respect the domains
    return getSwapMove(RandomGenerator.nextInt(variables.length),RandomGenerator.nextInt(variables.length))
  }
  
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    val idx = selectMax(rng2, (i: Int) => variableViolation(i).value);
    getBest(List(idx),rng2)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    getBest(rng2,rng2)
  }
  def getBest(rng1:Iterable[Int],rng2:Iterable[Int]): Move = {
    val bestSwap = selectMin2(rng1, rng2, (idx:Int,next:Int) => getSwapMove(idx,next).value,(idx:Int,v:Int) => variables(idx).dom.contains(variables(v).value) && variables(v).dom.contains(variables(idx).value) )
    val swap = bestSwap match { case (i1,i2) => getSwapMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    val bestMove = if(!closed){
      selectMin2(rng1,alldoms._1 to alldoms._2,(idx:Int,v:Int) => getAssignMove(idx,v).value,(idx:Int,v:Int) => variables(idx).dom.contains(v))
    }else{
      selectMin2(rng1,vals,(idx:Int,v:Int) => getAssignMove(idx,v).value,(idx:Int,v:Int) => variables(idx).dom.contains(v))
    }
    val move = bestMove match {case (i1,i2) => getAssignMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    if(swap.value < move.value) swap else move
  }
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}

//TODO: Take into account fixed variables!
class ThreeOpt(variables: Array[CBLSIntVarDom], objective: CBLSObjective, cs: ConstraintSystem, val offset: Int) extends Neighbourhood(variables){
  
  val variableViolation: Array[CBLSIntVar] = variables.map(cs.violation(_)).toArray
  val rng = offset until (offset+variables.length)
  reset();
  
  def reset(){
    //TODO: Add some randomization
    //TODO: Ensure that the domains are respected!
    for(i <- rng){
      vars(i) := (i)%variables.length+offset 
    }
    //println(variables.map(v => v.value).mkString(","))
  }
  def vars(idx:Int): CBLSIntVarDom = {
    variables(idx-offset)
  }
  def randomMove(it:Int):Move = {
    val idx = rng(RandomGenerator.nextInt(variables.length))
    val next = rng(RandomGenerator.nextInt(variables.length))
    getMove(idx,next)
  }
  
  def getMove(idx: Int, nextnext: Int):Move = {
    if(idx==nextnext)return new NoMove(Int.MaxValue)//would break the chain
    val next = vars(idx).value
    if(next==nextnext)return new NoMove(Int.MaxValue)//would break the chain
    val k = vars(next).value
    val last = vars(nextnext).value
    val list = List((vars(idx),k),(vars(next),last),(vars(nextnext),next))
    val obj = objective.assignVal(list)
    return new AssignsMove(list,obj);
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val idx = selectMax(rng, (i: Int) => variableViolation(i-offset).value, (i: Int) => nonTabu.contains(vars(i)));
    val next = selectMin(rng.toList.filter(i => nonTabu.contains(vars(i))))(next =>getMove(idx,next).value)
   // println(idx +  " "+ getMove(idx,vars(idx).value))
    getMove(idx,next)
    
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    
    //println(variables.map(v => v.value).mkString(","))
    //this one removes a node and reinsert it somewhere else
    //if(nonTabu.isEmpty)println("%EMPTY NON TABU")
    val rng2 = rng.toList.filter(i => nonTabu.contains(vars(i)));
    val res = selectMin2(rng2, rng2, (idx:Int,next:Int) => getMove(idx,next).value)
    //println(res +  " "+ (getMove _).tupled(res))//)._1,res._2))
    res match {
      case (idx,next) => getMove(idx,next)
      case _ => new NoMove(Int.MaxValue) //res is null when the NON TABU list is empty
    }
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}

class ThreeOptSub(variables: Array[CBLSIntVarDom], objective: CBLSObjective, cs: ConstraintSystem, offset: Int) extends ThreeOpt(variables, objective, cs, offset){
  //needed to add the allloop variable to be able to reinsert into the chain when the chain is only 1 element long.
  var allloop = false;
  override def reset(){
    super.reset();
    allloop = false;
  }
  override def getMove(idx: Int, nextnext: Int):Move = {
    val next = vars(idx).value
    val k = vars(next).value
    val last = vars(nextnext).value
    
    if(idx==next&&last==nextnext&& !allloop){
      //println("oops")
      return new NoMove(Int.MaxValue)//cannot join two selfloops unless there are only selfloops
    } 
    if(idx==nextnext&&idx==next){
      return new NoMove(Int.MaxValue)//cannot insert a selfloop into itself
    }
    //println("yep")
    if(idx==next&&idx!=nextnext){//idx is not in the chain, this should be an inclusion of idx after nextnext
      
      val list = List((vars(idx),last),(vars(nextnext),idx))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => allloop = false);
    }
    if(last==nextnext&&idx!=nextnext){//nextnext is not in the chain, this should be an inclusion of nextnext after idx
      val list = List((vars(idx),nextnext),(vars(nextnext),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => allloop = false);
    }
    if(idx==nextnext&&idx!=next){//assumes this is a removal of next
      val list = List((vars(idx),k),(vars(next),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => if(idx==k){
       // println(allloop)
        allloop = true
        //println(variables.map(v => v.value).mkString(","))
      });
    }
    if(next==nextnext&&idx!=next){//assumes this is a removal of next
      val list = List((vars(idx),k),(vars(next),next))
      return new BeforeMove(new AssignsMove(list,objective.assignVal(list)),
                          () => if(idx==k){
        //println(allloop)
        allloop = true
        //println(variables.map(v => v.value).mkString(","))
      });
    }
    val list = List((vars(idx),k),(vars(next),last),(vars(nextnext),next))
    return new AssignsMove(list,objective.assignVal(list));
  }
}


class MaxViolating(searchVariables: Array[CBLSIntVarDom], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables) {
  
  val indexRange = 0 until searchVariables.length;
  val variableViolation: Array[CBLSIntVar] = searchVariables.map(constraintSystem.violation(_)).toArray
  
  
  def reset() = {
    for (v: CBLSIntVar <- searchVariables)
      v.setValue(v.domain(RandomGenerator.nextInt(v.domain.length)))
  }
  def randomMove(it: Int): Move = {
    val bestIndex = indexRange(RandomGenerator.nextInt(indexRange.length))
    val bestValue = searchVariables(bestIndex).getRandValue()
    val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); 
    return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    //TODO: Only takes into account the violation!
    val bestIndex = selectMax(indexRange, (i: Int) => variableViolation(i).value, (i: Int) => nonTabu.contains(searchVariables(i)));
    val bestValue = selectMin(searchVariables(bestIndex).getDomain())((i: Int) => objective.assignVal(searchVariables(bestIndex), i), _ != searchVariables(bestIndex).value)
    val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); 
    return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    var minSoFar = Int.MaxValue;
    var bestPair = List.empty[(Int, Int)]
    for (violVar <- indexRange if nonTabu.contains(searchVariables(violVar)) && searchVariables(violVar).domainSize < 10000000) {
      for ((valueInDomain: Int) <- searchVariables(violVar).getDomain()) {
        if (searchVariables(violVar).value != valueInDomain) {
          val viol = objective.assignVal(searchVariables(violVar), valueInDomain)
          if (viol < minSoFar || bestPair.isEmpty) {
            bestPair = List((violVar, valueInDomain))
            minSoFar = viol
          } else if (!bestPair.isEmpty && viol == minSoFar) {
            bestPair = (violVar, valueInDomain) :: bestPair
          }
        }
      }
    }
    if (!bestPair.isEmpty) {
      val (selectedVar, selectedVal) = bestPair.apply(RandomGenerator.nextInt(bestPair.length))
      val bestIndex = selectedVar
      val bestValue = selectedVal
      val minObjective = objective.assignVal(searchVariables(bestIndex), bestValue); ;
      return new AssignMove(searchVariables(bestIndex),bestValue,minObjective)
    } else {
      return new NoMove(Int.MaxValue);
    }
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}


//This neighborhood assumes that all variables have the same domain
class MaxViolatingSwap(searchVariables: Array[CBLSIntVarDom], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables) {
  val indexRange = 0 until searchVariables.length;
  val variableViolation: Array[CBLSIntVar] = searchVariables.map(constraintSystem.violation(_)).toArray
  
  def reset() = {

  }
  def randomMove(it: Int): Move = {
    val bestIndex1 = indexRange(RandomGenerator.nextInt(indexRange.length))
    val bestIndex2 = indexRange(RandomGenerator.nextInt(indexRange.length))
    val minObjective = objective.swapVal(searchVariables(bestIndex1), searchVariables(bestIndex2))
    return new SwapMove(searchVariables(bestIndex1), searchVariables(bestIndex2),minObjective);
  }
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    //TODO: Only takes into account the violation for the first
    val bestIndex1 = selectMax(indexRange, (i: Int) => variableViolation(i).value, (i: Int) => nonTabu.contains(searchVariables(i)));
    val bestIndex2 = selectMin(indexRange)((i) => objective.swapVal(searchVariables(bestIndex1), searchVariables(i)), (i: Int) => searchVariables(i).value != searchVariables(bestIndex1).value && i != bestIndex1);
    if (!(nonTabu.contains(searchVariables(bestIndex1))) ||
      bestIndex1 == bestIndex2 || searchVariables(bestIndex2).value == searchVariables(bestIndex1).value) {
      return new NoMove(Int.MaxValue);
    } else {
      val minObjective = objective.swapVal(searchVariables(bestIndex1), searchVariables(bestIndex2))
      return new SwapMove(searchVariables(bestIndex1), searchVariables(bestIndex2),minObjective);
    }
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    getMinObjective(it, nonTabu);
  }
  
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}



//This neighborhood is not totally "holes in the domain"-proof!
class AllDifferent(searchVariables: Array[CBLSIntVarDom], objective: CBLSObjective, constraintSystem: ConstraintSystem) extends Neighbourhood(searchVariables) {
  /**/
  
  
  val (constants,variables) = searchVariables.partition((x) => x.minVal==x.maxVal)
  val indexRange: Range =0 until variables.length;
  val variableViolation: Array[CBLSIntVar] = variables.map(constraintSystem.violation(_)).toArray
  var freeValues: Set[Int] = Set.empty[Int]
  val (minVal,maxVal) = variables.foldLeft((Int.MaxValue ,Int.MinValue))((acc,v) => (math.min(acc._1,v.minVal),math.max(acc._2,v.maxVal)))
  reset();
  
  def reset() = {
    freeValues = Set.empty[Int]
    for (i <- minVal to maxVal) {
      freeValues += i;
    }
    for (c <- constants) {
      freeValues -= c.value;
    }
    for (v <- variables) {
      var value = v.getRandValue();
      while(!freeValues.contains(value)) value = v.getRandValue()
      v := value;
      freeValues -= value;
    }
  }
  
  def getSwapMove(idx1: Int,idx2: Int) = {
    val v1 = searchVariables(idx1).value
    val v2 = searchVariables(idx2).value
    if(searchVariables(idx1).dom.contains(v2) && searchVariables(idx2).dom.contains(v1))
      new SwapMove(searchVariables(idx1), searchVariables(idx2),objective.swapVal(searchVariables(idx1), searchVariables(idx2)));
    else new NoMove()
  }
  
  def getAssignMove(idx: Int, v: Int) = {
    if(searchVariables(idx).dom.contains(v) && freeValues.contains(v))
      new BeforeMove(new AssignMove(searchVariables(idx), v,objective.assignVal(searchVariables(idx), v)),
                     () => {freeValues -= v; freeValues += variables(idx).value});
    else new NoMove()
  }
  def randomMove(it: Int): Move = {
    if (freeValues.size == 0  || RandomGenerator.nextBoolean()) {
      val selectedIndex1 = indexRange(RandomGenerator.nextInt(indexRange.length))
      val selectedIndex2 = indexRange(RandomGenerator.nextInt(indexRange.length))
      getSwapMove(selectedIndex1,selectedIndex2)
    } else {
      val selectedIndex = indexRange(RandomGenerator.nextInt(indexRange.length))
      val selectedValue = variables(selectedIndex).getRandValue()
      getAssignMove(selectedIndex,selectedValue)
    }
  }
  
  def getMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    val idx = selectMax(rng2, (i: Int) => variableViolation(i).value);
    getBest(List(idx),rng2)
  }
  def getExtendedMinObjective(it: Int, nonTabu: Set[CBLSIntVar]): Move = {
    val rng2 = (0 until variables.length).toList.filter(i => nonTabu.contains(variables(i)));
    getBest(rng2,rng2)
  }
  def getBest(rng1:Iterable[Int],rng2:Iterable[Int]): Move = {
    val bestSwap = selectMin2(rng1, rng2, (idx:Int,next:Int) => getSwapMove(idx,next).value,(idx:Int,v:Int) => variables(idx).dom.contains(variables(v).value) && variables(v).dom.contains(variables(idx).value) )
    val swap = bestSwap match { case (i1,i2) => getSwapMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    val bestMove = selectMin2(rng1,freeValues,(idx:Int,v:Int) => getAssignMove(idx,v).value,(idx:Int,v:Int) => variables(idx).dom.contains(v))
    val move = bestMove match {case (i1,i2) => getAssignMove(i1,i2) case _ => new NoMove(Int.MaxValue)}
    if(swap.value < move.value) swap else move
  }
  def violation() = { variableViolation.foldLeft(0)((acc, x) => acc + x.value) };
}