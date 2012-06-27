package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestImplication extends FunSuite with ShouldMatchers with CPModel {

  
  test("=>1") {
      
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  cp.add(res == 0)
	  A.isBoundTo(1) should be(true)
	  B.isBoundTo(0) should be(true)
  }  
  
  test("=>2") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(A,B))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  }
	  nbSol should be(4)
  }
  
  test("=>3") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(res,A,B))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  }
	  nbSol should be(4)
  } 
  
    test("=>4") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(res,B,A))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  }
	  nbSol should be(4)
  } 
    
  test("=>5") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(B,A))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  }
	  nbSol should be(4)
  }
  
    test("=>6") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  cp.add(A ==> B)
	  cp.add(B == 0)
	  A.isBoundTo(0) should be(true)
  }
  

    
    
  
 
  


}