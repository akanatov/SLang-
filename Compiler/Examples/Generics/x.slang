

//b is Any
//a is A[Any]
//b is A[A[Any]]

a0 is A
a1 is A[Any]
any is Any
a2 is A[any]
a3 is A[rtn foo]

foo do end

unit Any end 
unit A end

unit A[G] end
unit A[c: Any] end
unit A[r: rtn] end


// A[H] is stored in the same file as A[G]
//unit A[H] end
