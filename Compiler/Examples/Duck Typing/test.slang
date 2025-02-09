unit A
	a, b: Integer
	A(:=a, :=b)
	require
		a > b
	none

	foo do end
require
   a > b
end
type B
	a, b: Integer
	B(:=a, :=b)
	require
		a < b
	none
	foo do end
require
   a < b
end
abstract type C
	a: Integer abstract
	b: Integer abstract
	foo abstract
require
	a /= b 
end

a is A(6, 5)
b is B (5, 6)
var c: C is a 
/* This duck-typing assignment adds A invariant to C inv using OR (no need to add repeatedly) and 
the same is done for pre and post conditions of all C routines which match A routines. 
Smart analysis is too tricky (add only to routines which are really called after duck-assignment)
*/
c.foo
c := b
/* This duck-typing assignment adds B invariant to C inv using OR (no need to add repeatedly) and 
the same is done for pre and post conditions of all C routines which match B routines. 
Smart analysis is too tricky
*/

c.foo

abstract type Any 
end
val type Integer
end


/* Duck assignment
	lvalue := expression
	lvalue has type type T and expression has type S  (Target <= Source)
	1. for every exported member of T there is only one exported member in S with the same or wider export status and signature in S which conforms to signature in T
	2. for every exported member routine of T its precondition and postcondition is extended using OR with the appropriate precondition and postcondition of routine from S
	3. invariant of T is extended with invariant of S using OR
*/

