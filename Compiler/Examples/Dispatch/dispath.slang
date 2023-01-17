unit Any end
c is C d is D f is F g is G i is I k is K
while a in (c, d, f, g, i, k) do
  a.f1
end
var gg is g.load // Dynamically load X or Y
gg.f1

// used at compile time in the program code
unit A
	f1 do end
end
unit B extend A
	f2 do end
	override f1 do end
end
unit C extend B
end
unit D extend B
	override f2 do end
end
unit E extend B, H
	override f1 do end
end
unit F extend E
end
unit G extend E, I
	override E.f3, I.f1
	load: as this foreign
end
unit H extend A
	f3 do end
end
unit I extend H, J
	override f3 do end
end
unit J extend A
	f4 do end
end
unit K extend J
	override f4 do end
end

// loaded dynamically during execution
unit X extend G
end
unit Y extend G
	override f1 do end
	override f4 do end
end
