d is Derived [Any]

type Base alias NewBase[G, H]
	f2 do end
end

type Derived [G] : NewBase [G, Any]
	override f2 do end
end

type Any
	f1 do end
end