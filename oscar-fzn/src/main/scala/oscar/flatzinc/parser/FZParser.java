/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/**
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.parser;

import java.io.FileInputStream;
import java.io.IOException;
import oscar.flatzinc.ParsingException;
import org.antlr.v4.runtime.*;


public class FZParser {
	public static Model readFlatZincModelFromFile(String fname){
		try{
		ANTLRInputStream input = new ANTLRInputStream(new FileInputStream(fname));
		//UnbufferedCharStream input = new UnbufferedCharStream(new FileInputStream(fname));
		return readFlatZincModel(input);
		}catch(IOException e){
	        System.err.println("File " + fname + " not found. Aborting.");
			System.exit(1);
		}
		return null;
	}
	public static Model readFlatZincModelFromString(String content){
		ANTLRInputStream input = new ANTLRInputStream(content);
		return readFlatZincModel(input);
	}
	public static Model readFlatZincModel(CharStream input){
		Model m = new Model();
		try{
	        FlatzincLexer lex = new FlatzincLexer(input);
			CommonTokenStream tokens = new CommonTokenStream(lex);
			FlatzincParser p = new FlatzincParser(tokens, m);
			p.setBuildParseTree(false);//in order to get acceptable performance on large files
	        p.flatzinc_model();
	        if(p.getNumberOfSyntaxErrors()>0){
	        	System.err.println("Syntax error. Aborting.");
	            System.err.println("If the flatzinc file is correct, please report to the developers.");
	        	System.exit(1);
	        }
			return m;
		}catch(RecognitionException e){
			e.printStackTrace();//TODO: report more friendly messages
			System.err.println("Syntax error. Aborting.");
	          System.err.println("If the flatzinc file is correct, please report to the developers.");
			System.exit(1);
		}catch(ParsingException e){
		  e.printStackTrace();//TODO: report more friendly messages
          System.err.println("Syntax error. Aborting.");
          System.err.println("If the flatzinc file is correct, please report to the developers.");
          System.exit(1);
		}
		return null;
	}
}
