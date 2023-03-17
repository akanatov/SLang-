d is Derived [Any]

unit Base alias NewBase[G, H]
	f2 do end
end

unit Derived [G] extend NewBase [G, Any]
	override f2 do end
end

unit Any
	f1 do end
end