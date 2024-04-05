 type  A
	a: Array [Any]
	foo1 do
		a := new Array[Integer]
	end
	foo2 do
		a := new Array[String]
	end
	append (e: Any)  do
		a (a.count + 1) := e
	end
	A do
	    new a 
	end
end

a is new A
a.foo1
a.foo2
a.append (1234)
a.append ("Hi covariant generics")
