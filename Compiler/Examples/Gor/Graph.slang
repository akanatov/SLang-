k is K
j is J
f is F
c is C
g is G
i is I
d is D
unit A
	f1 do end
end
unit B  extend  A
	override f1 do end
	f2 do end
end
unit C extend B
end
unit D extend B
	override f2	do end
end
unit E extend B, H
	override f1 do end
end
unit F extend E
end
unit G extend E, I
	override E.f3, I.f1
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

unit Any end
