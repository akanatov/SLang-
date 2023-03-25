a is A

unit A extend B
	override foo do end
	override foo (p: Any) do end
end

unit B
	foo do end
	goo: B
end

abstract unit Any
end