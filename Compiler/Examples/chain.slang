
a: ?A
b: ?B
c: ?C

// Instead of this
if a is A do 
	b := a.b
	if b is B do 
		c := b.c
		if c is C do 
			d is c.d
		else
			action_C_is_void
		end // if
	else
		action_B_is_void
	end // if
else
	action_A_is_void
end // if

/*
// 1. Shorter form is suggested. Minus - if several entitiies will have the same type ?T else case will not distingush such cases
do safe a.b.c.d 
else case
	?A: action_A_is_void
	?B: action_B_is_void
	?C: action_C_is_void
end

// 2. Shorter form is suggested. Minus - only one else action is avaialble
do safe a.b.c.d 
else 
	else_Action
end

do safe a.b.c.d end // No else action


// Similar approach may work for expressions too
x is do safe a.b.c.d else else_Value end

// If type of 'd' is detachable then else ? may be skipped
y is do safe a.b.c.d end // Type of y is equal to type of d and should be detachable


unit A
	const proc1 is () do end
	proc2 do end
	proc3 is () do end
	
	const proc11 is rtn () do end
	proc21 do end
	proc31 is rtn () do end

	const proc21 is @() do end
	proc22 do end
	proc32 is @() do end

	
	const f1 is (): Type do end
	f2: Type do end
	f3 is () : Type do end	
	f4 is (): (): Type do end
	
	const f11 is rtn (): Type do end
	f21: Type do end
	f31 is rtn () : Type do end	
	f41 is rtn (): rtn (): Type do end

	const f21 is @(): Type do end
	f21: Type do end
	f31 is @() : Type do end	
	f41 is @(): @(): Type do end
	
end

proc1 is A.proc1
proc2 is A.proc2
proc3 is A.proc3
proc1() // call
proc2() // call
proc3() // call

f1 is A.f1 // get rtn object
f2 is A.f2 // get rtn object
f3 is A.f3 // get rtn object

t1 is f1 // t1 type is rtn type
t4: (): Type is f1 // type of t4 is rtn
t2: Type is f1() // call f1, t2 type is Type
t3 is f1() // call f1, t3 type is Type

x1 is A.caveat // x1 type is rtn type rtn: rtn: Type
x2: (): Type is A.caveat() // call A.caveat as the type of x2 is rtn: Type
x3 is A.caveat() // call A.caveat
x4: (): (): Type is A.caveat // type of t4 is rtn

The problem
a: (T1, T2, T3) // That is a tuple!
vs
a: rtn (T1, T2, T3) // That is a rtn!

unit Base
	isSomething: Boolean abstract
end
unit Derived1 extend Base
	override isSomething: Boolean // variable attribute
end
unit Derived2 extend Base
	override isSomething: Boolean do end // function
end
unit Derived3 extend Base
	override const isSomething: Boolean is true // constant attribute
end

var base: Base is Derived1()
var bool is base.isSomething ()
base := Derived2()
bool := base.isSomething ()
base := Derived3()
bool := base.isSomething ()


unit T0
	foo (<parameters>): Result do return Result end
end // unit T0
active unit T1 extend T0
end
active unit T2 extend T0
end
active unit T3 extend T0
end

resultingTuple is in (T1, T2, T3) do foo (<arguments>) // Variant #1
resultingTuple is do foo (<arguments>) in (T1, T2, T3) // Variant #2
resultingTuple is do in (T1, T2, T3).foo (<arguments>) // Variant #3
resultingTuple is in (T1, T2, T3).foo (<arguments>)// Variant #3
// Type of 'resultingTuple' is tuple (Result, Result, Result)

while t in (T1, T2, T3) do
	result is t.foo // Calling a function leads to synchronization!
	// Type of 'result' is Result
end
*/