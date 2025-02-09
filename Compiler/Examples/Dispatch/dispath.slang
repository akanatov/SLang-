unit Any end
c is C d is D f is F g is G i is I k is K
while a in (c, d, f, g, i, k) do
  a.f1
end
var gg is g.load // Dynamically load X or Y
gg.f1

// used at compile time in the program code
type A
	f1 do end
end
type B : A
	f2 do end
	override f1 do end
end
type C : B
end
type D : B
	override f2 do end
end
type E : B, H
	override f1 do end
end
type F : E
end
type G : E, I
	override E.f3, I.f1
	load: as this foreign
end
type H : A
	f3 do end
end
type I : H, J
	override f3 do end
end
type J : A
	f4 do end
end
type K : J
	override f4 do end
end

// loaded dynamically during execution
type X : G
end
type Y : G
	override f1 do end
	override f4 do end
end
