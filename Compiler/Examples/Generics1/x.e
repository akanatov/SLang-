class B [G] feature
	attr: G
end

class A [G]  inherit  B[G] B[Any]
end 

class Test
create
	main
feature
	main is
	local
		a: A[Any]
		b: A[Integer]
	do
		create a
		create b 
	end
end

