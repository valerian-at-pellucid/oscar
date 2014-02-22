package oscar.cp.test.minizinc

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import oscar.cp.minizinc.Options
import oscar.cp.minizinc.NewParser
import oscar.cp.minizinc.FlatZinc2OscaR
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.FileOutputStream
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import scala.io.Source
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPIntVar

class TestBuild extends FunSuite with ShouldMatchers {
  

	test("Test fzn models") {
	  var ineclipse = false
	  
	  val pwd = new java.io.File(".").getCanonicalPath
	  // 
	  val modelFileList = 
	  if (ineclipse) new File("minizinc/test/flatzinc").listFiles.filter(_.getName.endsWith(".fzn")).sorted
	  else new File(pwd+"/../minizinc/test/flatzinc").listFiles.filter(_.getName.endsWith(".fzn")).sorted

	  println("testing ..."+modelFileList.mkString(","))
	  val old = Console.out;
	  for(f <- modelFileList) {
	    
	    old.println("Testing: " + f.getName())
	    
	    // redirect Console.out
	    var baos = new ByteArrayOutputStream();
	    var ps = new PrintStream(baos);
	    Console.setOut(ps);
	    
	    val args = Array[String]("-a", f.toString())
	    
	    FlatZinc2OscaR.parse(args)
	    
	    // restore Console.out
	    Console.out.flush();
	    Console.setOut(old);
	    	   
	    // write to file
	    //var os: OutputStream = new FileOutputStream (pwd+"/minizinc/testBuildOutput_"+f.getName().dropRight(4)); 
	    //baos.writeTo(os);
	    
	    val str = baos.toString().lines.toList

	    val output = 
	      if (ineclipse) Source.fromFile("minizinc/test/output/"+f.getName().dropRight(4)+".output").getLines.toList
	      else Source.fromFile(pwd+"/../minizinc/test/output/"+f.getName().dropRight(4)+".output").getLines.toList
	    
	    //println(str.mkString("\n"))
	    str.length should be(output.length)
	    //println(str.length + " --- " + output.length)
	    
	    for ((o, s) <- output.zip(str)) {
//	      println(o + " ---- " + s)
	      o should be(s)
	    }
	    
	    ps.close()
	    baos.close()
	  }
	  
	  
	}
	/*
	test("Test fzn models") {
	  val pwd = new java.io.File(".").getCanonicalPath
	  val modelFileList = new File(pwd+"/minizinc/test/flatzinc").listFiles.filter(_.getName.endsWith(".fzn")).sorted
	  val old = Console.out;
	  
	  for(f <- modelFileList; if f.getName().contains("GridColoring")) {
	    println("Testing: " + f.getName())
	  
	    val args = Array[String]("-a", f.toString())
	    
	    FlatZinc2OscaR.parse(args)

	  }
	}	
	*/
 	
}
