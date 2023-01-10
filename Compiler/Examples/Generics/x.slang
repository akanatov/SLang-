//build Generics from "."
//end


//b is Any
//a is A[Any]
//b is A[A[Any]]

//a0 is A
//foo do
	a1 is A[Any]
	any is Any
	a2 is A[any]
	a3 is A[rtn foo]
	a4 is A[Any, a .. b]
	a5 is A[Any, (a, b)]
//end

foo do end

unit Any end 
unit A end

unit A[G] 
	A do
		A(Any)
	end
	A(p: Any) do
	end
end
unit A[G, x:Any] end
unit A[G, x:(Any, Any)] end
unit A[c: Any] end
unit A[r: rtn] end


// A[H] is stored in the same file as A[G]
//unit A[H] end
