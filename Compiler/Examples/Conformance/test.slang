a is A

type A : B
	override foo do end
	override foo (p: Any) do end
end

type B
	foo do end
	goo: B
end

abstract type Any
end