a is A
type A : B, C
	override B.foo
end

type B
	foo do end
end

type C
	foo do end
end

abstract type Any
end