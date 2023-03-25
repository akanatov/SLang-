k is K
j is J
f is F
c is C
g is G
i is I
d is D
unit A
	f1 do end // f1@A[f1$A]
end
unit B  extend  A
	override f1 do end // f1@B[f1$A]
	f2 do end // f2@B[f2$B]
end
unit C extend B
end
unit D extend B
	override f2	do end // f2@D[f2$B]
end
unit E extend B, H
	override f1 do end // f1@E[f1$A]
end
unit F extend E
end
unit G extend E, I
	override E.f3, I.f1 // f3$
end
unit H extend A
	f3 do end // f3@H[f3$H]
end
unit I extend H, J
	override f3 do end // f3@I[f3$H]
end
unit J extend A
	f4 do end // f4@J[f4$J]
end
unit K extend J
	override f4 do end // f4@K[f4$J]
end

abstract unit Any end

/*
				f1$A	f4$J	f3$H	f2$B

	!0  - `A`:	f1@A[f1$A]
	!1  - `B`:	f2@B[f2$B]	*f1@B[f1$A]
	#2  - `E`:	f3@H[f3$H]	f2@B[f2$B]	override f1@E[f1$A]
	#3  - `F`:	f3@H[f3$H]	f2@B[f2$B]	override f1@E[f1$A]
	#4  - `G`:	override f3@I[<Void>]	f2@B[f2$B]	override f1@E[f1$A]
	#5  - `C`:	f2@B[f2$B]	override f1@B[f1$A]
	#6  - `D`:	override f2@D[f2$B]	override f1@B[f1$A]
	!7  - `H`:	f3@H[f3$H]	f1@A[f1$A]
	#8  - `I`:	f4@J[f4$J]	override f3@I[f3$H]	f1@A[f1$A]
	!9  - `J`:	f4@J[f4$J]	f1@A[f1$A]
	#10 - `K`:	override f4@K[f4$J]	f1@A[f1$A]


*/
