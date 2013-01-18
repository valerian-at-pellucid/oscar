package oscar.cp.mem.measures

import oscar.cp.mem.pareto.ParetoSet

object Hypervolume {

  def hypervolume[Sol](set: ParetoSet[Sol]): Double = {
    var prevObj2 = set.nadir(1)
    var volume = 0.0
    val sortedSet = set.sortedByObj(0)
    for (s <- sortedSet) {
      val obj1 = s.objs(0)
      val obj2 = s.objs(1)
      val dObj2 = prevObj2 - obj2
      val dObj1 = set.nadir(0) - obj1
      prevObj2 = obj2
      val v = ((dObj2.toDouble / 10000) * (dObj1.toDouble / 10000))
      if (v > 0) volume += v
    }
    volume
  }
}

