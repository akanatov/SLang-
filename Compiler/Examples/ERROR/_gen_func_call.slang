type A
	foo do
		foo[T, U, V].call
	end
end 
foo(1)

foo[T](1) // NOT SUPPOPRTED !!!
foo[T](a) // NOT SUPPOPRTED !!!

foo [T] (a: Type) do end


