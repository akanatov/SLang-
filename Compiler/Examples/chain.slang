
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


