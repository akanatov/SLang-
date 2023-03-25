k is K
j is J
f is F
c is C
g is G
i is I
d is D
unit A
	f1 do end // f1$A, f1@A
end
unit B  extend  A
	override f1 do end // f1$A:f1@B
	f2 do end // f2$B:f2@B
end
unit C extend B
end
unit D extend B
	override f2	do end // f2$B:f2@D
end
unit E extend B, H
	override f1 do end // f1$A:f1@E
end
unit F extend E
end
unit G extend E, I
	override E.f3, I.f1 // f3$
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

abstract unit Any end

/*
				f1$A	f4$J	f3$H	f2$B
	#0 - `A` has 1 parents: Any; has 3 children: B/3,H/2,J/2; members: f1@A
	#1 - `J` has 1 parents: A; has 2 children: I/1,K/0; members: f4@J, f1@A
	#2 - `K` has 1 parents: J; members: override f4@K, f1@A
	#3 - `I` has 2 parents: J H; has 1 children: G/0; members: override f3@I, f4@K, f1@A
	#4 - `G` has 2 parents: I E; override: I.f1,E.f3 ????
	#5 - `H` has 1 parents: A; has 2 children: E/2,I/1; members: f3@H, f1@A
	#6 - `E` has 2 parents: H B; has 2 children: G/0,F/0; members: override f1@E, f3@H
	#7 - `F` has 1 parents: E;
	#8 - `B` has 1 parents: A; has 3 children: E/2,C/0,D/0; members: f2@B, override f1@B
	#9 - `D` has 1 parents: B; members: override f2@D
	#10 - `C` has 1 parents: B;

*/
