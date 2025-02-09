a is A
type A : B
	override B.foo
	override foo (p: T1) do end
	override foo (p: T2) do end
end
type B
end
abstract type Any
end
type T1
end
type T2
end
