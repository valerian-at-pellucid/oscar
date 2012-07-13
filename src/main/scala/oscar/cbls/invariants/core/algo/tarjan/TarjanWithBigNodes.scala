/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.invariants.core.algo.tarjan



object test{
  def main(args: Array[String]) {

    val A = testnode("a")
    val B = testnode("b")
    val C = testnode("c")
    val D = testnode("d")

    A.succeedingnodes = List(B,C,D)
    B.succeedingnodes = List(C)
  //  C.succeedingnodes = List(B,D)
    D.succeedingnodes = List(A)
    println(TarjanWithBigNodes.getStronlyConnexComponents(List(A,B,C,D),(n:testnode) => n.succeedingnodes))

  }
}

case class testnode(name:String,var succeedingnodes:List[testnode] = List.empty) extends TarjanNode {
  override def toString:String = name
}

trait TarjanNode{
  var Index:Int = -1
  var LowLink:Int = -1
  var OnStack:Boolean = false
}

object TarjanWithBigNodes{

  def getStronlyConnexComponents[T <: TarjanNode](Nodes:Iterable[T], GetSucceedingNodes:(T => Iterable[T])):List[List[T]] = {
    var index:Int=0
    var Stack:List[T]=List.empty
    var Components:List[List[T]]= List.empty

    def visit(v:T){
      v.Index = index
      v.LowLink = index
      index +=1
      Stack = v::Stack
      v.OnStack = true

      // Consider successors of v
      for(w <- GetSucceedingNodes(v)){
        if(w.Index == -1){
          // Successor w has not yet been visited; recurse on it
          visit(w)
          v.LowLink = v.LowLink.min(w.LowLink)
        }else if(w.OnStack){
          // Successor w is in stack S and hence in the current SCC
          v.LowLink = v.LowLink.min(w.Index)
        }
      }

      // If v is a root node, pop the stack and generate an SCC
      if (v.LowLink == v.Index){
        //start a new strongly connected component
        var SCC:List[T] = List.empty
        var finished:Boolean = false
        while(!finished){
          val node = Stack.head
          Stack = Stack.tail
          node.OnStack = false
          SCC = node::SCC
          finished = (node == v)
        }
        Components = SCC :: Components
      }
    }

    for(n <- Nodes) {if(n.Index == -1) visit(n)}

    Components;
  }
}
