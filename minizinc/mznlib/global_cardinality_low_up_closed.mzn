predicate global_cardinality_low_up_closed(array[int] of var int: x, array[int] of int: cover, 
		array[int] of int: lb, array[int] of int: ub) = oscar_global_cardinality_low_up_closed(x, cover, lb, ub);

predicate oscar_global_cardinality_low_up_closed(array[int] of var int: x, array[int] of int: cover, 
		array[int] of int: lb, array[int] of int: ub);