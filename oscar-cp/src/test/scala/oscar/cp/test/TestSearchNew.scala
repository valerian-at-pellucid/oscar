package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

class TestSearchNew extends FunSuite with ShouldMatchers {

  test("ids search, bug #36") {
    val cp = new CPSolver()

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    val x = Array(CPIntVar(0)(cp))

    cp.minimize(x(0)) subjectTo () search {
      new BinaryFirstFailBranching(x)
    }
    val stat = cp.start()
    stat.nSols should be(1)
    nbSol should be(1)
  }

  test("explocompleted") {
    val cp = CPSolver()

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    val x = Array.tabulate(3)(i => CPBoolVar()(cp))

    cp.search {
      new BinaryStaticOrderBranching(x)
    }

    cp.start(nSols = 3).completed should be(false)
    cp.start().completed should be(true)
    cp.start(nSols = 3).completed should be(false)
    cp.start().completed should be(true)
    cp.start(failureLimit = 3).completed should be(false)
  }

  test("timelimit") {
    val cp = CPSolver()
    val x = Array.fill(40)(CPIntVar(0 to 1)(cp))

    var t0 = System.currentTimeMillis()
    cp.solve subjectTo {} search {
      new BinaryStaticOrderBranching(x)

    }

    val stat = cp.start(timeLimit = 2)
    val time: Int = ((System.currentTimeMillis() - t0) / 1000).toInt
    time should be >= (2)
    stat.completed should be(false)
    time should be <= (4)
    stat.completed should be(false)
  }

  test("optimize") {

    val cp = new CPSolver()
    val x = CPIntVar(Array(1, 5, 9, 10))(cp)
    cp.minimize(x) subjectTo {}

    var best = 0
    cp.onSolution { best = x.value }

    cp.search {
      new BinaryStaticOrderBranching(Array(x), _.max)
    }
    val stat = cp.start()
    stat.nSols should be(4)
    best should be(1)

  }

  test("test 2 dfs") {

    val cp = CPSolver()
    val x = Array.fill(2)(CPIntVar(1 to 2)(cp))
    val y = Array.fill(2)(CPIntVar(1 to 2)(cp))

    //def dom(x: CPIntVar) = (x.min to x.max).filter(x.hasValue(_))

    var nbSol = 0
    cp.onSolution { nbSol += 1 }

    cp.search {
      new BinaryFirstFailBranching(x) ++ new BinaryFirstFailBranching(y)
    }

    val stat = cp.start()
    nbSol should equal(16)
    stat.nSols should be(16)
  }
}
