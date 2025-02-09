//foo foreign
 type  A alias XX [G] 
	: M, B, ~C 
	//use A as BB, B, C const A, B, C
	select	b, foo(as this, rtn):rtn (as this), c
override M.f, B[as this].moo, A.foo (as this, rtn):rtn (as this)
//override M.foo, M.goo
	new M(as this, rtn (as this): as anchor), B, C

/*const:
end*/
some do end 
{}:
too do end
{this}:
a, b, c: A
{Any}:
{A, B, C}:
f_u: A do end
/*foo
do
end*/

end // A

/*
 type  AF[foo: rtn(), H init (), G extend F, N: Integer, K extend T init (as this)] 
end

use const A, B, C
return
raise
return
break
? identifier
final  type  AA
end
ref  type  B
end
val  type  C
end
active  type  D
end
extend  type  E
end
final  type  A1
end
final ref  type  B1
end
final val  type  C1
end
final active  type  D1
end
 */