package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core.CPStore
import oscar.cp.core.CPVarInt

class TestVariable extends FunSuite with ShouldMatchers {

  trait StoreAndVariables {
    val store = new CPStore()
    val a = CPVarInt(1, 6)(store)
    val b = CPVarInt(0, 31)(store)
    val c = CPVarInt(1, 10)(store)
  }

  test("test a") {
    new StoreAndVariables {
      a.removeValue(2)
      a.removeValue(4)
      store.pushState()
      a.removeValue(2)
      a.removeValue(10)
      a.removeValue(6)
      store.pushState()
      a.removeValue(-2)
      a.removeValue(4)
      a.removeValue(1)
      a.removeValue(4)
      store.pop()
      a.removeValue(1)
      a.removeValue(0)
      store.pop()
      //[1, 3, 5, 6] card:4 min:1 max:6
      assert(a.min == 1)
      assert(a.max == 6)
      assert(a.size == 4)
      assert(a.hasValue(1))
      assert(a.hasValue(3))
      assert(a.hasValue(5))
      assert(a.hasValue(6))
      assert(!a.hasValue(2))
    }
  }

  test("test b") {
    new StoreAndVariables {
      b.assign(0)
      assert(b.isBound && b.value == 0)
    }
  }

  test("test c") {
    new StoreAndVariables {
      c.removeValue(5)
      c.updateMax(7)
      //1..4,6..7
      var values = List(1, 2, 3, 4, 6, 7)
      for (v <- c.min until c.max) {
        if (c.hasValue(v)) {
          assert(v == values.head)
          values = values.tail
        }
      }
    }
  }

  test("test d") {
    val cp = new CPStore()
    val x = CPVarInt(1, 6)(cp)

    assert(x.valueAfter(5) == 6)
    assert(x.valueAfter(-10) == 1)
    val v: Int = x.valueAfter(6)
    assert(v == 6)

    val y = CPVarInt(-100, 100)(cp)
    y.removeValue(0)

    assert(y.valueAfter(-1) == 1)
    assert(y.valueAfter(0) == 1)
    assert(y.valueBefore(-1) == -2)
    assert(y.valueBefore(1) == -1)
    assert(y.valueBefore(0) == -1)
    assert(y.valueBefore(1000) == 100)

    y.removeValue(30)
    y.removeValue(31)
    y.removeValue(32)
    y.removeValue(33)

    assert(y.valueBefore(31) == 29)
    assert(y.valueBefore(34) == 29)
    assert(y.valueAfter(31) == 34)
    assert(y.valueAfter(34) == 35)
  }

  // Non determinist test !
  test("test e") {
    val cp = new CPStore()
    val freq = Array.fill(4)(0)
    val x = CPVarInt(Set(0, 1, 2, 3))(cp)
    for (i <- 0 until 200) {
      freq(x.randomValue) += 1
    }
    for (i <- 0 until 4) {
      assert(freq(i) > 0)
    }
    println(freq.mkString(", "))
  }

  test("test f") {
    val cp = new CPStore()
    val x = CPVarInt(Set(1, 5, 9, 10))(cp)
    val y = CPVarInt(Set(5, 9, 11))(cp)
    val z = CPVarInt(Set(6, 7, 11))(cp)
    val w = CPVarInt(14)(cp)

    assert(x.intersectionSize(y) == 2)
    assert(y.intersectionSize(x) == 2)

    assert(z.intersectionSize(y) == 1)
    assert(y.intersectionSize(z) == 1)

    assert(w.intersectionSize(x) == 0)
    assert(x.intersectionSize(w) == 0)
  }
}
