

//b is Any
//a is A[Any]
//b is A[A[Any]]

a1 is A[Any]
any is Any
a2 is A[any]
//a3 is A[rtn do end]


unit A[G] end

// A[H] is stored in the same file as A[G]
//unit A[H] end

unit A[c: Any] end

unit A[r: rtn] end

unit Any end 

