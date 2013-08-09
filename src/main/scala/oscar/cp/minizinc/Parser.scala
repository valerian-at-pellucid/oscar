package oscar.cp.minizinc

import scala.util.parsing.combinator._
import FZType._
import oscar.cp.modeling.CPSolver
import oscar.cp.core._
import oscar.cp.modeling._
import java.io.FileReader

class Parser extends JavaTokenParsers {// RegexParsers {
	var model : Minizinc_model = new Minizinc_model
	val cp = CPSolver()
	cp.silent = true
	
	//def myParseAll(input: String) = {parseAll(var_decl, input)}
	def myParseAll(input: FileReader) = {parseAll(flatzinc_model, input)}
	def myParseAll(input: String) = {parseAll(flatzinc_model, input)}
	//def myParseAll(input: String) = {parseAll(constraint, input)}
	def parseParam(input: String) = {parseAll(param_decl, input)}
	def parseVar(input: String) = {parseAll(var_decl, input)}
	
	def flatzinc_model : Parser[Any] = rep(pred_decl)~rep(param_decl)~rep(var_decl)~rep(constraint)~solve_goal
	def pred_decl : Parser[Any] = "predicate"~identifier~"("~rep1sep(pred_param, ",")~");"

	def identifier : Parser[Any] = index_set
	
	def pred_param : Parser[Any] = pred_param_type~":"~pred_ann_id // what about no space before the ":" and one after ?
	def pred_param_type : Parser[Any] = par_pred_param_type | var_pred_param_type	
	
	def par_type : Parser[Any] = (
	    "bool"
		| "float"
		| "int"
		| "set of int"
		| "array ["~index_set~"] of bool"
		| "array ["~index_set~"] of float"
		| "array ["~index_set~"] of int"
		| "array ["~index_set~"] of set of int"
	)
	def par_pred_param_type : Parser[Any] = (
		par_type
		| float_const~".."~float_const
		| int_const~".."~int_const
		| "{"~rep1sep(int_const, ",")~"}"
		| "set of"~int_const~".."~int_const
		| "set of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~float_const~".."~float_const
		| "array ["~index_set~"] of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~"set of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_type : Parser[Any] = (
	    "var"~int_const~".."~int_const
		| "var float"
	    | "var"~float_const~".."~float_const
	    | "var int"
	    | "var"~int_const~".."~int_const
	    | "var"~"{"~rep1sep(int_const, ",")~"}"
	    | "var set of"~int_const~".."~int_const
	    | "var set of"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var bool"
	    | "array ["~index_set~"] of var float"
	    | "array ["~index_set~"] of var"~float_const~".."~float_const
	    | "array ["~index_set~"] of var int"
	    | "array ["~index_set~"] of var"~int_const~".."~int_const
	    | "array ["~index_set~"] of var"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var set of"~int_const~".."~int_const
	    | "array ["~index_set~"] of var set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_pred_param_type : Parser[Any] = (
		var_type
		| "var set of int"
		| "array ["~index_set~"] of var set of int"
	)
	def index_set : Parser[Any] = (
	    "1.."~int_const ^^ {
	      case "1.."~i => Range(1, i+1, 1)
	    }
	    | "int" ^^ (_.toString())// what about the fact that "int" is only allowed in predicates ?
	)
	
	def expr : Parser[Any] = ( //need to find a way to return something else than any
		bool_const
		| set_const //should be float -> int -> set, inverted for set to work, need testing
		| float_const
		| int_const //^^ (_.toInt) how to avoid the casts when using an expr that is an Any
		| var_par_id~"["~int_const~"]" ^^ {
		  case id~"["~i~"]" => List(id, i)
		}
		| var_par_id
		| array_expr
		| annotation
		| "...string constant..." //???
	)
	def pred_ann_id : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	def var_par_id : Parser[String] = "-*[A-Za-z][A-Za-z0-9_]*".r
	
	
	//definition of the constants
	
	def bool_const : Parser[Boolean] = (
	    "true" ^^ (x => true)
	    | "false" ^^ (x => false)
	)
	def float_const : Parser[Float] = (
	    int_const~"."~"[0-9][0-9]*".r~opt("[eE]".r~int_const) ^^ {
	      case i1~"."~i2~exp => exp match {
	        case Some(e~i3) => (i1+"."+i2+e+i3).toFloat
	        case None => (i1+"."+i2).toFloat
	      }
	    }
	    | int_const~"[eE]".r~int_const ^^ {
	      case i1~e~i2 => (i1+e+i2).toFloat
	    }
	)
	def int_const : Parser[Int] = "[+-]?[0-9][0-9]*".r ^^ (_.toInt)// [+-] at the begining of the regex in the grammar, what does that mean ?
	def set_const : Parser[Any] = ( 
	    int_const~".."~int_const ^^ { 
	    	case int1~".."~int2 => Range(int1, int2+1, 1) 
	    } 
	    | "{"~>rep1sep(int_const, ",")<~"}" 
	)
	
	def array_expr : Parser[List[Any]] = (
	    //"[]" | not useful since repsep is used instead of rep1sep
	    "["~>repsep(expr, ",")<~"]"
	)
	
	
	//Parameter and variable declarations
	
	def param_decl : Parser[Any] = par_type~":"~var_par_id~"="~expr~";" ^^ 
	{
	  case tp~":"~id~"="~e~";" =>
	    tp match {
	      case "bool" => model.dict += 
	        ((id, (FZType.P_BOOL, 
	            new ParamBool(e.toString.toBoolean, id))))
	      case "int" => model.dict += 
	        ((id, (FZType.P_INT, 
	            new ParamInt(e.toString.toInt, id))))
	      case "float" => model.dict += 
	        ((id, (FZType.P_FLOAT,
	            new ParamFloat(e.toString.toFloat, id)))) // ! no floats in oscar, what to do ?
	      case "set of int" => model.dict += 
	        ((id, (FZType.P_SET_INT,
	          e match {
	          	case x:Range => new ParamSetOfInt(e, true, id)
	          	case x:List[Any] => new ParamSetOfInt(e, false, id)
	          }
	        )))
	        
	      //code duplication can be easily avoided but what's better ?
	      case "array ["~iset~"] of bool" => model.dict += 
	        ((id, (FZType.P_ARRAY_BOOL, 
	            new ParamArrayBool(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of float" => model.dict += 
	        ((id, (FZType.P_ARRAY_FLOAT, 
	            new ParamArrayFloat(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of int" => model.dict += 
	        ((id, (FZType.P_ARRAY_INT, 
	            new ParamArrayInt(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of set of int" => model.dict +=
	        ((id, (FZType.P_ARRAY_SET_INT, 
	            new ParamArraySetOfInt(e,
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	    }
	}
	  //the expr has restriction... what about it ?
	
	def var_decl : Parser[Any] = var_type~":"~var_par_id~annotations~opt("="~expr)~";" ^^
	{ 
//to be done when assignment of var is understood
//	  case tp~":"~id~ann~Some("="~e)~";" => model.dict.get(id) match {
//	    case Some((tp, fzo)) => model.dict += ((id, (tp, fzo)))
//	  }
	  case tp~":"~id~ann~None~";" => 
	    tp match {
	      case "var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_INT_RANGE,
	            new VarIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann, 
	                CPVarInt(cp, i1.toString.toInt to i2.toString.toInt), id))))
	            //println("cpvar created")
	      case "var int" => model.dict += 
	      	((id, (FZType.V_INT, 
	      		new VarInt(ann, CPVarInt(cp, -10000, 10000), id)))) // what should I do when no assign ?
	      	//println(model.dict.toString)
	      case "array ["~iset~"] of var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_ARRAY_INT_R, 
	            new VarArrayIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann,
	                iset match {
	                	case x:Range => 
	                	  Array.fill(x.length){CPVarInt(cp, i1.toString.toInt to i2.toString.toInt)} 
	                	case _ => null
	            	}
	            	, id))))
	      case "array ["~iset~"] of var int" => model.dict +=
	        ((id, (FZType.V_ARRAY_INT,
	            new VarArrayInt(ann, 
	            	iset match {
	                	case x:Range => 
	                	  Array.fill(x.length){CPVarInt(cp, -10000, 10000)} 
	                	case _ => null
	            	}
	                , id))))
	        //println(model.dict.toString)
	      //case "array ["~iset~"] of var bool" =>
	    }
	    
	}// the vars in assignment must be declared earlier
	
	
	// Constraint declaration (every constraint should be a case below the match
	
	def constraint : Parser[Any] = "constraint"~pred_ann_id~"("~rep1sep(expr, ",")~")"~annotations~";" ^^ {
	  case "constraint"~cstr~"("~varList~")"~ann~";" => cstr match {
	    case "int_le" => 
	      var cpvar = Array[CPVarInt]()
	      //println(varList.toString)
	      //println(varList(0))
	      (varList(0), varList(1)) match {
	        case (x:Int, y:List[Any]) => {
	          model.dict.get(y(0).toString) match {
	            case Some((tp, fzo)) => {
	              //assert(tp == FZType.V_ARRAY_INT_R, "The FZObject doesn't have the type V_INT_RANGE")
	              cpvar :+= CPVarInt(cp, x.toInt)
	              tp match {
	                case FZType.V_ARRAY_INT_R => {
	                  cpvar :+= fzo.asInstanceOf[VarArrayIntRange].cpvar(y(1).toString.toInt-1)
	                }
	                case FZType.V_ARRAY_INT => {
	                  cpvar :+= fzo.asInstanceOf[VarArrayInt].cpvar(y(1).toString.toInt-1)
	                }
	              }
	            }
	            case _ => assert(false, "varList didn't contains enough varibles")
	          }
	          //println("int le cst added")
	        }
	        case (x:Int, y:String) => {
	          //println("is a is")
	          model.dict.get(y.toString) match {
	            case Some((tp, fzo)) => {
	              assert(tp == FZType.V_INT_RANGE, "The FZObject doesn't have the type V_INT_RANGE")
	              cpvar :+= CPVarInt(cp, x)
	              cpvar :+= fzo.asInstanceOf[VarIntRange].cpvar
	            }
	            case _ => assert(false, "varList didn't contains enough varibles")
	          }
	        }
	        //to be added later 
	        case (x:List[Any], y:Int) => {
	          model.dict.get(x(0).toString) match {
	            case Some((tp, fzo)) => {
	              //assert(tp == FZType.V_ARRAY_INT_R, "The FZObject doesn't have the type V_INT_RANGE")
	              tp match {
	                case FZType.V_ARRAY_INT_R => {
	                  cpvar :+= fzo.asInstanceOf[VarArrayIntRange].cpvar(x(1).toString.toInt-1)
	                }
	                case FZType.V_ARRAY_INT => {
	                  cpvar :+= fzo.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
	                }
	              }
	            }
	            case _ => assert(false, "varList didn't contains enough varibles")
	          }
	          cpvar :+= CPVarInt(cp, y.toInt)
	          
	        }
	        case (x:List[Any], y:List[Any]) => {
	          println("is a ll")
	          (model.dict.get(x(0).toString), model.dict.get(y(1).toString)) match {
		        case (Some((tp0, fzo0)), Some((tp1, fzo1))) => 
		          tp0 match {
	                case FZType.V_ARRAY_INT_R => {
	                  cpvar :+= fzo0.asInstanceOf[VarArrayIntRange].cpvar(x(1).toString.toInt-1)
	                }
	                case FZType.V_ARRAY_INT => {
	                  cpvar :+= fzo0.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
	                }
	              }
		          tp1 match {
	                case FZType.V_ARRAY_INT_R => {
	                  cpvar :+= fzo1.asInstanceOf[VarArrayIntRange].cpvar(y(1).toString.toInt-1)
	                }
	                case FZType.V_ARRAY_INT => {
	                  cpvar :+= fzo1.asInstanceOf[VarArrayInt].cpvar(y(1).toString.toInt-1)
	                }
	              }
		        case _ => assert(false, "varList didn't contains enough varibles")
	          }
	        }
	        case (x:List[Any], y:String) => {
	          println("is a ls")
	        }
	        case (x:String, y:Int) => {
	          println("is a si")
	          model.dict.get(x.toString) match {
	            case Some((tp, fzo)) => {
	              assert(tp == FZType.V_INT_RANGE, "The FZObject doesn't have the type V_INT_RANGE")
	              cpvar :+= fzo.asInstanceOf[VarIntRange].cpvar
	              cpvar :+= CPVarInt(cp, y)
	            }
	            case _ => assert(false, "varList didn't contains enough varibles")
	          }
	        }
	        case (x:String, y:List[Any]) => {
	          println("is a sl")
	        }
	        case (x:String, y:String) => {
	          println("is a ss")
	          (model.dict.get(x.toString), model.dict.get(y.toString)) match {
		        case (Some((tp0, fzo0)), Some((tp1, fzo1))) => 
		          assert(tp0 == FZType.V_INT_RANGE, "The FZObject 0 doesn't have the type V_INT_RANGE")
		          assert(tp1 == FZType.V_INT_RANGE, "The FZObject 1 doesn't have the type V_INT_RANGE")
		          cpvar :+= fzo0.asInstanceOf[VarIntRange].cpvar
		          cpvar :+= fzo1.asInstanceOf[VarIntRange].cpvar
		        case _ => assert(false, "varList didn't contains enough varibles")
	          }
	        }
	      }
	      cp.add(cpvar(0) <= cpvar(1))
	    case "int_ne" => // /!\ VARSETOFINT must be in arg, not VARINT
	      var x = Array[CPVarInt]()
	      if(varList(0).isInstanceOf[List[Any]] && varList(1).isInstanceOf[List[Any]]) {
	        varList.asInstanceOf[List[List[Any]]].foreach { e =>
	          model.dict.get(e(0).toString) match {
	          	case Some((tp, fzo)) => 
	          		assert(tp == FZType.V_ARRAY_INT_R, "The fzo doesn't have the type V_ARRAY_INT_R")
	          		x :+= fzo.asInstanceOf[VarArrayIntRange].cpvar(e(1).toString.toInt-1)
	          	}
	        }
	      }
	      else {
	        (model.dict.get(varList(0).toString), model.dict.get(varList(1).toString)) match {
	        	case (Some((tp0, fzo0)), Some((tp1, fzo1))) => 
	        		assert(tp0 == FZType.V_INT_RANGE, "The FZObject 0 doesn't have the type V_INT_RANGE")
	        		assert(tp1 == FZType.V_INT_RANGE, "The FZObject 1 doesn't have the type V_INT_RANGE")
	        		//cp.add(fzo0.asInstanceOf[VarIntRange].cpvar != fzo1.asInstanceOf[VarIntRange].cpvar)
	        		x :+= fzo0.asInstanceOf[VarIntRange].cpvar
	        		x :+= fzo1.asInstanceOf[VarIntRange].cpvar
	        		//println(x.mkString(","))
	        		//println("lit")
	        		//println("Constraint int_ne added")
	        	case _ => assert(false, "varList didn't contains enough varibles")
	        }
	      }
	      cp.add(x(0) != x(1))
	      
	    case "int_lin_ne" =>
	      int_lin_cstr(varList, ann, cstr)
	    case "int_lin_eq" => 
	      int_lin_cstr(varList, ann, cstr)
	    case "int_lin_le" =>
	      int_lin_cstr(varList, ann, cstr)

	  }
	}
	
	def int_lin_cstr(varList: List[Any], ann: Any, cstr: String) {
	  assert(varList.length == 3, "To many arguments for int_lin_ne")
	      var cpvar = Array[CPVarInt]()
	      varList(1).asInstanceOf[List[Any]].foreach { e =>
	        e match {
	          case x:List[Any] => {
	            model.dict.get(x(0).toString) match {
		          case Some((tp, fzo)) => 
		            tp match {
		                case FZType.V_ARRAY_INT_R => {
		                  cpvar :+= fzo.asInstanceOf[VarArrayIntRange].cpvar(x(1).toString.toInt-1)
		                }
		                case FZType.V_ARRAY_INT => {
		                  cpvar :+= fzo.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
		                }
		            }
		        }
	          }
	          case x:Int => {
	            cpvar :+= CPVarInt(cp, x)
	          }
	          case x:String => {
	            model.dict.get(x) match {
		          case Some((tp, fzo)) => 
		            tp match {
		                case FZType.V_INT => {
		                  cpvar :+= fzo.asInstanceOf[VarInt].cpvar
		                }
		                case FZType.V_INT_RANGE => {
		                  cpvar :+= fzo.asInstanceOf[VarIntRange].cpvar
		                }
		            }
	            }
	          }
	        }
	      }
	      var cst: Array[Int] = varList(0).asInstanceOf[List[Int]].toArray
	      cstr match {
	        case "int_lin_ne" => 
	          cp.add(weightedSum(cst, cpvar) != varList(2).toString.toInt)
	        case "int_lin_eq" =>
	          cp.add(weightedSum(cst, cpvar) == varList(2).toString.toInt)
	        case "int_lin_le" => 
	          cp.add(weightedSum(cst, cpvar) <= varList(2).toString.toInt) 
	      }
	      //println(cstr + " added")
	}
	
	def solve_goal : Parser[Any] = (
	    "solve"~annotations~"satisfy;" ^^ { 
	      case "solve"~ann~"satisfy;" => solver("sat", null)
	      /*
	      case _ => //println(model.dict.toString) 
	      var x = Array[CPVarInt]()
	      var state = Array[VarState]()
	      //var name = Array[String]() // only used for formating the output
	      var output: Boolean = false // only used for formating the output
	      var c = 0
	      //var array = Array[Boolean]()
	      //var previousName: String = null
	      //var first = true // only used for formating the output
	      model.dict.foreach { e => 
	        //println(x.mkString(","))
	        e._2 match {
	          case (tp, fzo) => // /!\ not always CPVarInt
	            //println(fzo.asInstanceOf[VarIntRange].cpvar + " " + fzo.asInstanceOf[VarIntRange].name)
	            tp match {
	              case FZType.V_INT_RANGE => {
	                x :+= fzo.asInstanceOf[VarIntRange].cpvar
	                fzo.asInstanceOf[VarIntRange].annotations.foreach { ann =>
	            		if ( ann.name == "output_var" ) { output = true }
	                }
	                state :+= new VarState(fzo.asInstanceOf[VarIntRange].name,
	                    output, false, false, 1)
	                output = false
	                //name :+= fzo.asInstanceOf[VarIntRange].name
	                //println(output.length + "  " + x.length)
	                //if ( output.length < x.length) { output :+= false }
	                //array :+= false
	                
	              }
	              case FZType.V_ARRAY_INT_R => {
	                //var c = 0
	                var first = true
	                fzo.asInstanceOf[VarArrayIntRange].cpvar.foreach { e =>
	                	x :+= e
	                	fzo.asInstanceOf[VarArrayIntRange].annotations.foreach { ann =>
	            			if ( ann.name == "output_array" ) { output = true }
	                	}
	                	state :+= new VarState(fzo.asInstanceOf[VarArrayIntRange].name,
	                    output, true, first, fzo.asInstanceOf[VarArrayIntRange].range.length)
	                	
	                	first = false
	                	/*
	                	//name :+= fzo.asInstanceOf[VarArrayIntRange].name + "[" + c.toString + "]"
	                	name :+= fzo.asInstanceOf[VarArrayIntRange].name
	                	fzo.asInstanceOf[VarArrayIntRange].annotations.foreach { ann =>
	            			if ( ann.name == "output_array" ) { output :+= true }
	                	}
	                	if ( output.length < x.length) { output :+= false }
	                	array :+= true
	                	c += 1
	                	* 
	                	*/
	                }
	              }
	              case FZType.V_INT => {
	                
	              }
	              case FZType.V_ARRAY_INT => {
	                
	              }
	              case _ => {
	                println("The type " + tp.toString() + " is not supported/relevant for the solver")
	              }
	            }
	            
	        }
	        //println("done")
	      }
	      //println(x.mkString(","))
	      cp.solve subjectTo {
	      } exploration {
	        cp.binary(x)
	        Range(0, x.length, 1).foreach { i =>
	        	if ( state(i).output ) { 
	        	  if ( state(i).array ) {
	        	    c += 1
	        	    if (state(i).first) {
	        	    	print(state(i).name + " = array1d(1.." + state(i).size + ", [" + x(i).toString.split(" ")(1))
	        	    } else if( c == state(i).size ) {
	        	    	println("," + x(i).toString + "]);")
	        	    	c = 0
	        	    } else {
	        	    	print("," + x(i).toString)
	        	    }
	        	  } else {
	        	  //if(!first) { print(" ")}
	        	  	println(state(i).name + " =" + x(i).toString + ";") 
	        	  }
	        	  //first = false // only used for formating the output
	        	}
	        }
	        //first = true // only used for formating the output
	        println("----------")
	        //println(x.mkString(","))
	      } run (1)
	      //} run (nbSolMax = 2)
	      println("==========")
	      //println(model.dict.toString)
	       * 
	       */
	    }
	    | "solve"~annotations~"minimize"~expr~";"
	    | "solve"~annotations~"maximize"~expr~";" ^^ { 
	      case "solve"~ann~"maximize"~e~";" => solver("max", e)
	    }
	) // expr must be a var name of var array element
	
	def solver(tp: String, expr: Any) {
	  var x = Array[CPVarInt]()
      var state = Array[VarState]()
      var output: Boolean = false // only used for formating the output
      var c = 0
      model.dict.foreach { e => 
        e._2 match {
          case (tp, fzo) => // /!\ not always CPVarInt
            tp match {
              case FZType.V_INT_RANGE => {
                x :+= fzo.asInstanceOf[VarIntRange].cpvar
                fzo.asInstanceOf[VarIntRange].annotations.foreach { ann =>
            		if ( ann.name == "output_var" ) { output = true }
                }
                state :+= new VarState(fzo.asInstanceOf[VarIntRange].name,
                    output, false, false, 1)
                output = false
                
              }
              case FZType.V_ARRAY_INT_R => {
                //var c = 0
                var first = true
                fzo.asInstanceOf[VarArrayIntRange].cpvar.foreach { e =>
                	x :+= e
                	fzo.asInstanceOf[VarArrayIntRange].annotations.foreach { ann =>
            			if ( ann.name == "output_array" ) { output = true }
                	}
                	state :+= new VarState(fzo.asInstanceOf[VarArrayIntRange].name,
                    output, true, first, fzo.asInstanceOf[VarArrayIntRange].range.length)
                	
                	first = false
                }
              }
              case FZType.V_INT => {
                
              }
              case FZType.V_ARRAY_INT => {
                
              }
              case _ => {
                println("The type " + tp.toString() + " is not supported/relevant for the solver")
              }
            }
            
        }
      }
	  tp match {
	    case "sat" => {
	      cp.solve subjectTo {
	      } exploration {
	        cp.binary(x)
	        format_output(x, state)
	      } run (1)
	      println("==========")
	    }
	    case "max" => {
	      var cpvar: CPVarInt = null
	      println(expr.toString)
	      expr match {
	        //case x:List[List[Any]] => //can oscar max several values,... can it be done in cp ?
	        case x:List[Any] => {
	          model.dict.get(x(0).toString) match {
		          case Some((tp, fzo)) => 
		            tp match {
		                case FZType.V_ARRAY_INT_R => {
		                  cpvar = fzo.asInstanceOf[VarArrayIntRange].cpvar(x(1).toString.toInt-1)
		                }
		                case FZType.V_ARRAY_INT => {
		                  cpvar = fzo.asInstanceOf[VarArrayInt].cpvar(x(1).toString.toInt-1)
		                }
		            }
		      }
	        }
	        case x:String => 
	          model.dict.get(x) match {
		          case Some((tp, fzo)) => 
		            tp match {
		                case FZType.V_INT => {
		                  cpvar = fzo.asInstanceOf[VarInt].cpvar
		                }
		                case FZType.V_INT_RANGE => {
		                  cpvar = fzo.asInstanceOf[VarIntRange].cpvar
		                }
		            }
	          }
	      }
	      cp.maximize(cpvar) subjectTo {
	        //what is the right way yo do it ? I dont know OscaR !
	      } exploration {
	        println(model.dict.toString)
	        cp.binary(x)
	        format_output(x, state)
	        println(model.dict.toString)
	      } run ()
	      println("==========")
	    }
	    case "min" =>
	  }   
	}
	
	def format_output(x: Array[CPVarInt], state: Array[VarState]) {
		var c = 0
		Range(0, x.length, 1).foreach { i =>
	    	if ( state(i).output ) { 
	    	  if ( state(i).array ) {
	    	    c += 1
	    	    if (state(i).first) {
	    	    	print(state(i).name + " = array1d(1.." + state(i).size + ", [" + x(i).toString.split(" ")(1))
	    	    } else if( c == state(i).size ) {
	    	    	println("," + x(i).toString + "]);")
	    	    	c = 0
	    	    } else {
	    	    	print("," + x(i).toString)
	    	    }
	    	  } else {
	    	  	println(state(i).name + " =" + x(i).toString + ";") 
	    	  }
	    	}
	    }
	    println("----------")
	}
	
	def annotations : Parser[List[Annotation]] = rep("::"~>annotation) 
	// is there a list of annotations ?
	def annotation : Parser[Annotation] = (
	    pred_ann_id~"("~rep1sep(expr, ",")~")" ^^ {
	      case ann~"("~list~")" => new Annotation(ann, list)
	    }
	    | pred_ann_id ^^ (new Annotation(_, null))
	)// some notes, see syntax
	
}