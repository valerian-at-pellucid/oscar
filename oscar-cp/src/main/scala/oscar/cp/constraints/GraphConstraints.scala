package oscar.cp.constraints

import oscar.cp.core.CPGraphVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 * Create Graph Basic Constraints to modify domains and call propagate when domain change
 */

class RequiresNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node required") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    G.addNodeToGraph(n)
  }

}

class ExcludesNode(val G: CPGraphVar, n: Int) extends Constraint(G.s, "Node excluded") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    G.removeNodeFromGraph(n)
  }

}

class RequiresEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge required") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    G.addEdgeToGraph(src,dest)
  }

}

class ExcludesEdge(val G: CPGraphVar, src: Int, dest: Int) extends Constraint(G.s, "Edge excluded") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    G.removeEdgeFromGraph(src,dest)
  }

}
