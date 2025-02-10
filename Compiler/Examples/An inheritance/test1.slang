type A
	f2 do end
	f3 do end
	f4 do end
end
type B: A
	f1 do end
	override f2 do end
	override f4 do end
	f5 do end
end
type C: A
	override f3 do end
	override f4 do end
end
type D: B
	override f1 do end
	override f4 do end
	override f5 do end
end
type Any end


a: A is D
a := C

