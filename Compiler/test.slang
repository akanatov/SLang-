//foo foreign
unit A alias XX [G] 
	extend M, B, ~C 
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
unit AF[foo: rtn(), H init (), G extend F, N: Integer, K extend T init (as this)] 
end

use const A, B, C
return
raise
return
break
? identifier
final unit AA
end
ref unit B
end
val unit C
end
concurrent unit D
end
extend unit E
end
final unit A1
end
final ref unit B1
end
final val unit C1
end
final concurrent unit D1
end
 */