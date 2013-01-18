package oscar.cp.mem.pareto

class ParetoSet[Sol](val nObjs: Int) {

  class SolNode(val sol: MOSol[Sol], val objsNode: Array[LinkedNode[SolNode]]) { override def toString = sol.toString }

  val Objs = 0 until nObjs
  
  var nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue)
  var ideal: Array[Int] = Array.fill(nObjs)(0)

  private val objsVal: Array[OrderedLinkedList[SolNode]] = Array.fill(nObjs)(OrderedLinkedList[SolNode]())
  private var X: List[SolNode] = List()

  private def deleteNode(s: SolNode) = {
    for (i <- Objs) {
      val prev = s.objsNode(i).prev
      val next = s.objsNode(i).next
      objsVal(i).remove(s.objsNode(i))

      if (prev != null) prev.solNode.sol.ub(i) = if (next != null) next.objVal
      else nadir(i)
      if (next != null) next.solNode.sol.lb(i) = if (prev != null) prev.objVal
      else ideal(i)
    }
  }

  private def realInsert(xNew: MOSol[Sol]) = {
    val solNode = new SolNode(xNew, Array.tabulate(nObjs)(i => objsVal(i).insert(xNew.objs(i), null)))
    for (i <- Objs) {
      val objNode = solNode.objsNode(i)
      objNode.solNode = solNode

      if (!objNode.hasPrev) solNode.sol.lb(i) = ideal(i)
      else {
        objNode.prev.solNode.sol.ub(i) = objNode.objVal
        solNode.sol.lb(i) = objNode.prev.objVal
      }

      if (!objNode.hasNext) solNode.sol.ub(i) = nadir(i)
      else {
        objNode.next.solNode.sol.lb(i) = objNode.objVal
        solNode.sol.ub(i) = objNode.next.objVal
      }
    }
    X = solNode :: X
  }

  def insert(xNew: MOSol[Sol]): Int = {
    if (X.isEmpty) {
      realInsert(xNew)
      return 0
    } else {
      val removed = clean(xNew)
      realInsert(xNew)
      return removed
    }
  }

  private def clean(xNew: MOSol[Sol]): Int = {
    var removed = 0
    def clean0(xNew: MOSol[Sol], l: List[SolNode]): List[SolNode] = l match {
      case Nil => Nil
      case x :: t => {
        if (xNew dominates x.sol) {
          removed += 1
          deleteNode(x)
          clean0(xNew, t)
        } else if (x.sol dominates xNew) {
          removed = -1
          x :: t
        } else x :: clean0(xNew, t)
      }
    }
    X = clean0(xNew, X)
    removed
  }

  def size = X.size

  def isEmpty = X.isEmpty

  def apply(i: Int) = X(i)

  def map[B](f: (MOSol[Sol]) => B): IndexedSeq[B] = X.map(n => f(n.sol)).toIndexedSeq

  def mkString(s: String) = X.mkString(s)

  def foreach[B](f: (MOSol[Sol]) => B) = X.foreach(n => f(n.sol))

  def clear() { X = List() }

  def filter(f: (MOSol[Sol]) => Boolean) = X.map(_.sol).filter(f)

  def toArray: Array[MOSol[Sol]] = X.map(_.sol).toArray

  def sortBy(f: (MOSol[Sol]) => Int) = X.map(_.sol).sortBy(f)

  def max(f: (MOSol[Sol]) => Int): MOSol[Sol] = min(-f(_))
  
  def sortedByObj(obj: Int) = objsVal(obj).toList.map(_.sol)

  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    var minValue = Int.MaxValue
    var min: MOSol[Sol] = null
    for (x <- X) {
      val value = f(x.sol)
      if (value < minValue) {
        minValue = value
        min = x.sol
      }
    }
    min
  }

  override def toString: String = X.toString
}

object ParetoSet {
  def apply[Sol](nObjs: Int) = new ParetoSet[Sol](nObjs)

  def main(args: Array[String]) {

    val set = ParetoSet[String](2)

    val s1 = MOSol[String]("sol1", 2, 5)
    set.insert(s1)
    println(computeDiffSurf(s1))
    val s2 = MOSol[String]("sol2", 5, 2)
    set.insert(s2)
    println(computeDiffSurf(s2))

    val s3 = MOSol[String]("solFinal", 2, 3)
    set.insert(s3)
    println(computeDiffSurf(s3))
    val s4 = MOSol[String]("solFinal", 3, 2)
    set.insert(s4)
    println(computeDiffSurf(s4))

  }

  def computeDiffSurf(sol: MOSol[String]): Int = {

    val diffD0 = sol.upperBound(0) - sol.objs(0)
    val diffD1 = sol.objs(1) - sol.lowerBound(1)

    val diffU0 = sol.objs(0) - sol.lowerBound(0)
    val diffU1 = sol.upperBound(1) - sol.objs(1)

    
    diffD0 * diffD1 + diffU0 * diffU1
  }
}