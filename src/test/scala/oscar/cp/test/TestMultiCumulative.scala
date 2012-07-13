package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling.CumulativeActivity

import org.scalacheck._

class TestMultiCumulative extends FunSuite with ShouldMatchers with CPModel {
	
	// This test depends of the tie break of the priority queue eventPointSeries
	test("Test 1 : Event generation 1 (tie break dependent)") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(CPVarInt(cp, 1 to 2), // start
										CPVarInt(cp, 2 to 3), // duration
										CPVarInt(cp, 3 to 5), // end
										CPVarInt(cp, 0 to 0), // machine
										CPVarInt(cp, 2 to 2)) // resource
		
		val tasks = Array(t1)
		val capacities = Array(4)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		constraint.generateEventPointSeries(0)
		
		
		// Event test
		var event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getEST)
		event.increment should be(t1.getMaxResource)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isPruningEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getEST)
		event.increment should be(0)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLST)
		event.increment should be(1)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getECT)
		event.increment should be(-1)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLCT)
		event.increment should be(-t1.getMaxResource)
		
		constraint.eventPointSeries.size should be(0)
	}
	
	// This test depends of the tie break of the priority queue eventPointSeries
	test("Test 2 : Event generation 2 (tie break dependent)") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(CPVarInt(cp, 1 to 2), // start
										CPVarInt(cp, 2 to 3), // duration
										CPVarInt(cp, 3 to 5), // end
										CPVarInt(cp, 0 to 0), // machine
										CPVarInt(cp, -2 to -2)) // resource
		
		val tasks = Array(t1)
		val capacities = Array(4)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		constraint.generateEventPointSeries(0)
		
		// Event test
		var event = constraint.eventPointSeries.dequeue
		event.isPruningEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getEST)
		event.increment should be(0)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLST)
		event.increment should be(1)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLST)
		event.increment should be(t1.getMaxResource)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getECT)
		event.increment should be(-t1.getMaxResource)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getECT)
		event.increment should be(-1)
		
		constraint.eventPointSeries.size should be(0)
	}
	
	test("Test 3 : Event generation 3") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(CPVarInt(cp, 1 to 3), // start
										CPVarInt(cp, 2 to 2), // duration
										CPVarInt(cp, 3 to 5), // end
										CPVarInt(cp, 0 to 0), // machine
										CPVarInt(cp, -2 to -2)) // resource
		
		val tasks = Array(t1)
		val capacities = Array(4)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		constraint.generateEventPointSeries(0)
		
		// Event test
		var event = constraint.eventPointSeries.dequeue
		event.isPruningEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getEST)
		event.increment should be(0)
		
		constraint.eventPointSeries.size should be(0)
	}
	
	//This test depends of the tie break of the priority queue eventPointSeries
	test("Test 4 : Event generation 4 (tie break dependent)") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(CPVarInt(cp, 1 to 1), // start
										CPVarInt(cp, 2 to 2), // duration
										CPVarInt(cp, 3 to 3), // end
										CPVarInt(cp, 0 to 0), // machine
										CPVarInt(cp, -2 to -2)) // resource
		
		val tasks = Array(t1)
		val capacities = Array(4)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		constraint.generateEventPointSeries(0)
		
		// Event test
		var event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLST)
		event.increment should be(1)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getLST)
		event.increment should be(t1.getMaxResource)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isProfileEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getECT)
		event.increment should be(-t1.getMaxResource)
		// Event test
		event = constraint.eventPointSeries.dequeue
		event.isCheckEvent should be(true)
		event.task should be(0)
		event.date should be(t1.getECT)
		event.increment should be(-1)
		
		constraint.eventPointSeries.size should be(0)
	}
	
	test("Test 5 : Adjust Min/Max") {
		
		val cp = CPSolver()
		
		val x = CPVarInt(cp, 1 to 4)
		
		var res = MultiCumulative.adjustMax(x, 3)
		
		res._1 should be(true)
		
		res = MultiCumulative.adjustMin(x, 2)
		
		res._1 should be(true)
		
		x.hasValue(1) should be(false)
		x.hasValue(4) should be(false)
		
		x.getMin should be(2)
		x.getMax should be(3)
		
		res = MultiCumulative.adjustMax(x, 10)
		
		res._1 should be(false)
		
		res = MultiCumulative.adjustMin(x, 0)
		
		res._1 should be(false)
		
		x.getMin should be(2)
		x.getMax should be(3)
	}
	
	test("Test 6 : Fix Var") {
		
		val cp = CPSolver()
		
		val x = CPVarInt(cp, 1 to 4)
		
		var res = MultiCumulative.fixVar(x, 3)
		
		res._1 should be(true)
		x.hasValue(3) should be(true)
		x.hasValue(1) should be(false)
		x.hasValue(4) should be(false)
		
		res = MultiCumulative.fixVar(x, 1)
		
		res._1 should be(false)
		x.hasValue(3) should be(true)
		x.hasValue(1) should be(false)
		x.hasValue(4) should be(false)
	}
	
	test("Test 7 : Remove Value") {
		
		val cp = CPSolver()
		
		val x = CPVarInt(cp, 1 to 4)
		
		var res = MultiCumulative.removeValue(x, 3)
		
		res._1 should be(true)
		x.hasValue(3) should be(false)
		x.hasValue(1) should be(true)
		x.hasValue(4) should be(true)
		
		res = MultiCumulative.removeValue(x, 1)
		
		res._1 should be(true)
		x.hasValue(3) should be(false)
		x.hasValue(1) should be(false)
		x.hasValue(4) should be(true)
		
		res = MultiCumulative.removeValue(x, 1)
		
		res._1 should be(false)
		x.hasValue(3) should be(false)
		x.hasValue(1) should be(false)
		x.hasValue(4) should be(true)
	}
	
	test("Test 8 : Sweeping") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(CPVarInt(cp, 1 to 2), // start
										CPVarInt(cp, 2 to 4), // duration
										CPVarInt(cp, 3 to 6), // end
										CPVarInt(cp, 0 to 0), // machine
										CPVarInt(cp, -1 to 1)) // resource
		
		val t2 = new CumulativeActivity(CPVarInt(cp, 0 to 6), // start
										CPVarInt(cp, 0 to 2), // duration
										CPVarInt(cp, 0 to 8), // end
										CPVarInt(cp, 0 to 1), // machine
										CPVarInt(cp, -3 to 4)) // resource
		
		val tasks = Array(t1, t2)
		val capacities = Array(4, 3)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		constraint.sweepAlgorithm(0)
		
		
	}
	
	test("Test 9 : Nicolas Beldiceanu example 1") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(new CPVarInt(cp, 1 to 2), // start
										new CPVarInt(cp, 2 to 4), // duration
										new CPVarInt(cp, 3 to 6), // end
										new CPVarInt(cp, 0 to 0), // machine
										new CPVarInt(cp, -1 to 1)) // resource
		
		val t2 = new CumulativeActivity(new CPVarInt(cp, 0 to 6), // start
										new CPVarInt(cp, 0 to 2), // duration
										new CPVarInt(cp, 0 to 8), // end
										new CPVarInt(cp, 0 to 1), // machine
										new CPVarInt(cp, -3 to 4)) // resource
		
		val tasks = Array(t1, t2)
		val capacities = Array(4, 3)
		val capacities2 = Array(10, 10)
		
		val constraint = new MultiCumulative(cp, tasks, capacities, true)
		
		cp.add(constraint)
		
		t2.getEST should be(1)
		t2.getLST should be(2)
		
		t2.getECT should be(3)
		t2.getLCT should be(4)
		
		t2.getMinDuration should be(1)
		t2.getMaxDuration should be(2)
		
		t2.getMinResource should be(3)
		t2.getMaxResource should be(4)
	}
}
