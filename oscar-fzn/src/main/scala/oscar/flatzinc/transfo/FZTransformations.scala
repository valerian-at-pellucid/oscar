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
package oscar.flatzinc.transfo

import scala.util.control.Breaks._
import oscar.flatzinc.model._
import oscar.flatzinc.cbls.Log
import scala.collection.mutable.{ Map => MMap, Set => MSet }

object FZModelTransfo {
  def findInvariants(implicit model: FZProblem, log:Log) = {
    //Find all free variables of the model.
    var freeVariables: List[Variable] =
      model.variables.filter((variable: Variable) =>
        //model.map(id).id == id  //to take each variable only once (as there may be aliases)
        //&&
        !variable.isDefined //to remove the ones already defined
        && variable.min != variable.max //to remove de facto constants
          //JNM: Removed the following because the CP search variables are maybe not the CBLS search variables. 
          //This makes a huge difference for the BACP for instance.
          //&&
          //!model.search.heuristics.exists((h) => h._1.exists((v: Variable) => v == model.map(id) && (v.max - v.min) < 1000)) // I am not sure if this is needed...
          )
    
          
    //Remove free variables which are defined by a eq constraint with a constant argument. 
    //Replaced this code by checking whether the domain is a singleton. above (after propagation of the domains)
    
          
    //Find all constraints which do not already define a variable and can define a variable (be turned into an invariant)
    
   
    
    //For all free variables
    for (v <- freeVariables.sortWith((x: Variable, y: Variable) => x.max - x.min > y.max - y.min)) {
      val cand = v.cstrs.filter((c: Constraint) => c.definedVar.isEmpty && c.canDefineVar && c.getCandidateDefVars().contains(v))
      val cand2 = cand//.filter(c => ! dependsOn(c,v,false))
      if(cand2.length > 1){
        log("%!! Found a variable that could be defined by more than one invariant:"+v+" "+cand2.toString)
      }
      //Select the first suitable constraint
      if(!cand2.isEmpty) cand2.head.setDefinedVar(v)
      
    }
  }
  def dependsOn(c: Constraint, v: Variable,test: Boolean = true): Boolean = {
    c.getVariables().exists(w => if (w.isDefined) c.definedVar!= Some(w) && dependsOn(w.definingConstraint.get,v) 
                                 else test && w == v)
  }

  
  
  
  
  def propagateDomainBounds(model: FZProblem) = {
    //TODO: Also reduce the domains from GCC_closed and other such constraints. Or do it from the minizinc to flatzinc level
    //TODO: Do more than just the bounds, then handle the in_set constraint here.
     val (cstrs,retract)= model.constraints.partition(c => 
      c match {
        case int_le(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value); false
        case int_le(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value); false
        case int_lt(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value+1); false
        case int_lt(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value-1); false
        case bool_le(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value); false
        case bool_le(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value); false
        case bool_lt(x:ConcreteConstant, y:ConcreteVariable, _) => y.dom.geq(x.value+1); false
        case bool_lt(x:ConcreteVariable, y:ConcreteConstant, _) => x.dom.leq(y.value-1); false
        case int_eq(x: ConcreteConstant, y: ConcreteVariable, ann) => y.dom.geq(x.value); y.dom.leq(x.value); false
        case int_eq(x: ConcreteVariable, y: ConcreteConstant, ann) => x.dom.geq(y.value); x.dom.leq(y.value); false
        case bool_eq(x: ConcreteConstant, y: ConcreteVariable, ann) => y.dom.geq(x.value); y.dom.leq(x.value); false
        case bool_eq(x: ConcreteVariable, y: ConcreteConstant, ann) => x.dom.geq(y.value); x.dom.leq(y.value); false
        //The cstrs below might need to be iterated until fixpoint...
        case int_le(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); x.dom.leq(y.max); true
        case int_lt(x:ConcreteVariable, y:ConcreteVariable, _ ) =>{
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          y.dom.geq(x.min+1); x.dom.leq(y.max-1); 
          //println(x+x.dom.toString()+"\t"+y+y.dom);
          true
        } 
        case int_eq(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); y.dom.leq(x.max); x.dom.geq(y.min); x.dom.leq(y.max); true
        case bool_eq(x:ConcreteVariable, y:ConcreteVariable, _ ) => y.dom.geq(x.min); y.dom.leq(x.max); x.dom.geq(y.min); x.dom.leq(y.max); true
        case _ => true 
      })
      model.constraints = cstrs
      for(c <- retract){
        c.retract()
      }
  }
  
  
  
  def getSortedInvariants(inv: Array[Constraint]): (Array[Constraint],List[Constraint]) = {
    val invariants = inv.toArray;
    var sorted = List.empty[Constraint];
    val mapping = MMap.empty[Constraint, Int];
    var heads = List.empty[Constraint]
    var removed = List.empty[Constraint]
    for (i <- invariants) {
      mapping += i -> i.getVariables.filter((v) => v.isDefined && (v.definingConstraint.get != i)).length;
      if(mapping(i)==0){
        heads = i :: heads;
        mapping.remove(i)
      }
    }
    def explore() = {
      while (!heads.isEmpty) {
        val k = heads.head
        heads = heads.tail
        sorted = k::sorted
        for(j <- k.definedVar.get.cstrs){
          if(mapping.contains(j) ){
            mapping(j) = mapping(j)-1
            if(mapping(j)==0){
              heads = j :: heads;
              mapping.remove(j)
            }
          }
        }
      }
    }
    explore()
    if(!mapping.isEmpty)println("% There is a cycle in the set of invariants.!"+mapping.size)
    while(!mapping.isEmpty){
      val (remc,value) = mapping.keys.foldLeft((null.asInstanceOf[Constraint],0))((best,cur) => {val curval = mapping(cur)/*cur.definedVar.get.cstrs.filter(c => c!=cur && mapping.contains(c) && mapping(c)==1).length*/; if(curval > best._2) (cur,curval) else best;});
      mapping.remove(remc)
      for(j <- remc.definedVar.get.cstrs){
        if(mapping.contains(j) ){
          mapping(j) = mapping(j) -1
          if(mapping(j)==0){
            heads = j :: heads;
            mapping.remove(j)
          }
        }
      }
      removed = remc :: removed
      remc.unsetDefinedVar(remc.definedVar.get)
      explore()
     // println(mapping.map{case (c,i) => (c,i,c.getVariables.filter(v => {val cc = v.definingConstraint.getOrElse(c); /*mapping.contains(cc) &&*/ cc!=c}).toList.map(v => v.definingConstraint.get )) }.mkString("\n"))      
    }
    return (sorted.reverse.toArray,removed);
  }
}