package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling._

import org.scalacheck._

class TestCumulativeResourceSet extends FunSuite with ShouldMatchers {
	
	test("Test 1: packing") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 4)
		val act2 = Activity(cp, 3)
		val act3 = Activity(cp, 2)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResourceSet(cp, 2, 2)
		
		act1.needs(resource, 0 to 1, 1)
		act2.needs(resource, 0 to 1, 1)
		act3.needs(resource, 0 to 1, 1)
		
		var nSol = 0
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.solveAll subjectTo {
			
			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(resource.resourcesOf(act3) == 0)	
			
		} exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		nSol should be(4)
	}
	
	test("Test 2: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 1 to 2)
		val act3 = Activity(cp, 1 to 4)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResourceSet(cp, 2, 1)
		
		act1.needs(resource, 0 to 1, 1)
		act2.needs(resource, 0 to 1, 1)
		act3.needs(resource, 0 to 1, 1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 4), (0, 4, 3), (1, 0, 4), (1, 4, 0), (2, 0, 1), (2, 1, 0))
				               
		cp.solveAll subjectTo {
			
			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(resource.resourcesOf(act3) == 0)	
			
		} exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act2.dur.value should be(1)
		act3.dur.value should be(1)
		nSol should be(6)
	}
	
	test("Test 3: height") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 4)
		val act2 = Activity(cp, 3)
		val act3 = Activity(cp, 2)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResourceSet(cp, 2, 2)
		
		act1.needs(resource, 0 to 1, 1 to 2)
		act2.needs(resource, 0 to 1, 1)
		act3.needs(resource, 0 to 1, 1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.solveAll subjectTo {
			
			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(resource.resourcesOf(act3) == 0)	
			
		} exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		resource.heightOf(act1).value should be(1)
		
		nSol should be(4)
	}
	
	test("Test 4: alternatives") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResourceSet(cp, 2, 2)
		
		act1.needs(resource, 0 to 1, 3)
		act2.needs(resource, 0 to 1, 2)
		act3.needs(resource, 0 to 1, 1)
		act4.supplies(resource, 0 to 1, 1)
		
		
		var nSol = 0
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.solveAll subjectTo {
			
			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(resource.resourcesOf(act3) == 0)	
			
		} 
		
		resource.resourcesOf(act4).value should be(0)
		
		cp.exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(2)
		act4.dur.value should be(3)
		nSol should be(2)
	}
	
	test("Test 5: alternatives") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResourceSet(cp, 2, 2)
		
		act1.needs(resource, 0 to 1, 4)
		act2.needs(resource, 0 to 1, 3)
		act3.needs(resource, 0 to 1, 1)
		act4.supplies(resource, 0 to 1, 1 to 2)
		
		var nSol = 0
		
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.solveAll subjectTo {
			
			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(resource.resourcesOf(act3) == 0)	
			
		} 
		
		resource.resourcesOf(act4).value should be(0)
		
		cp.exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(2)
		act4.dur.value should be(3)
		resource.heightOf(act4).value should be(-2)
		nSol should be(2)
	}
	
	test("Test 6: alternatives") {	
		
		val horizon = 6
		val cp = new CPScheduler(horizon)		
		val req = Array(1,4,3,2)
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResourceSet(cp, 2, 5)
		
		act1.needs(resource, Array(0, 1), 1)
		act2.needs(resource, Array(0, 1), 4)
		act3.needs(resource, Array(0, 1), 3)
		act4.needs(resource, Array(0, 1), 2)
		
		var nbSol = 0

		cp.solveAll 
		cp.subjectTo {
			
			cp.add(resource.resourcesOf(act2) == 0)
		} 
		
		resource.resourcesOf(act2).value should be(0)
		resource.resourcesOf(act3).value should be(1)
		resource.resourcesOf(act4).value should be(1)
		
		cp.exploration {
			
			cp.binary(acts.map(resource.resourcesOf(_)))
			nbSol += 1
		}	
		
		nbSol should be(1)
	}
	
	test("Test 7: alternatives") {	
		
		val horizon = 106
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val act5 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4, act5)
		
		val resource = CumulativeResourceSet(cp, 2, 5)
		
		act1.needs(resource, Array(0, 1), 1)
		act2.needs(resource, Array(0, 1), 4)
		act3.needs(resource, Array(0, 1), 3)
		act4.needs(resource, Array(0, 1), 2)
		act5.needs(resource, Array(0, 1), 1)

		cp.add(act1.start == 0)
		cp.add(act2.start == 0)
		cp.add(act3.start == 0)
		cp.add(act4.start == 0)  
			  
		cp.add(resource.resourcesOf(act2) == 0)	
		
		cp.addResourceConstraints()
		
		resource.resourcesOf(act1).value should be(0)
		resource.resourcesOf(act3).value should be(1)
		resource.resourcesOf(act4).value should be(1)
		
		resource.resourcesOf(act5).size  should be(2)
			
		cp.add(resource.resourcesOf(act5) == 0)
			
		act5.start.min should be(6)
	}
	
	test("Test 8: alternatives") {
		
		val horizon = 11
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 0 to 5)
		val act2 = Activity(cp, 6)
		val acts = Array(act1, act2)
		
		val resource = CumulativeResourceSet(cp, 2, 3)
		
		act1.needs(resource, Array(0, 1), 0 to 5)
		act2.needs(resource, Array(0, 1), 0 to 5)
		
		cp.add(act1.start == 0)

		cp.solveAll subjectTo {

			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.heightOf(act1) == 2)
			cp.add(resource.heightOf(act2) == 2)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(act2.start == 3)
		}
		
		act1.dur.min should be(0)
		act1.dur.max should be(3)
	}
}