package oscar.cp.minizinc

object FZType extends Enumeration {
    type FZType = Value
	val P_BOOL, 
		P_FLOAT, 
		P_INT,
		P_SET_INT,
		P_ARRAY_BOOL,
		P_ARRAY_FLOAT,
		V_BOOL,
		V_FLOAT,
		V_INT,
		V_INT_RANGE,
		V_ARRAY_INT_R,
		P_ARRAY_INT = Value
}