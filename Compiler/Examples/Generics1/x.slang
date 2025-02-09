a is A[Any]
type B
end
type A [G] : B, C[G], C[B]
	A do
		A (Any) 
		B ()
		C[G]
//		C // CTE: No such parent !!!
		C [Any]
		C [B]
		B
	end
	A (any: Any) do
	end
	foo do
		A // CTE: init call must be in the body of init only
		B () // CTE: init call must be in the body of init only
	end
end 
type C[G]
end
type Any
end
