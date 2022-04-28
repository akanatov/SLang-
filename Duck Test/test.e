-- 28 lines/ 363 chars
class Main create main feature
	main is local
		a: A
		b: B
		data: Integer
	do
		create a
		create b
		a.foo (data)
		b.foo (data)
	end
end
class A feature
	foo (par: Integer) is
	do
		if par = 0 then
			// raise exception
		end
	end
end
class B feature
	foo (par: Integer) is
	do
		if par < 0 then
			// raise exception
		end
	end
end


// 21 lines/241 chars
a is A
b is B
data is Integer
a.foo (data)
b.foo (data)
unit A
	foo (par: Integer)
	do
		if par = 0 do
			raise "Exception"
		end
	end
end
unit B
	foo (par: Integer)
	do
		if par < 0 do
			raise "Exception"
		end
	end
end

// 19 lines/256 chars
entry {
	let a = A{}
	let b = B{}
	let data = 0
	a.foo(data)
	a.foo(data)
}
type A = class {}
type B = class {}
fn A.foo (par: i32) {
	if par == 0 {
		panic ("Exception")
	}
}
fn B.foo (par: i32) {
	if par < 0 {
		panic ("Exception")
	}
}