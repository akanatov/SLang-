//goo (a, b, c: A) do {a, b, a, c} end
foo do
    a is 5
    var a is 5
end

foo1   foreign

unit A
	foo (a: A) foreign
	goo 
	do {a, b, c} 
		a is 5
	end
end
