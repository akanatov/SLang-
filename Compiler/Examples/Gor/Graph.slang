k is K
j is J
f is F
c is C
g is G
i is I
d is D
type A
	f1 do end // f1@A[f1$A]
end
type B  :  A
	override f1 do end // f1@B[f1$A]
	f2 do end // f2@B[f2$B]
end
type C : B
end
type D : B
	override f2	do end // f2@D[f2$B]
end
type E : B, H
	override f1 do end // f1@E[f1$A]
end
type F : E
end
type G : E, I
	override E.f3, I.f1 // f3$
end
type H : A
	f3 do end // f3@H[f3$H]
end
type I : H, J
	override f3 do end // f3@I[f3$H]
end
type J : A
	f4 do end // f4@J[f4$J]
end
type K : J
	override f4 do end // f4@K[f4$J]
end

abstract type Any end

/*
					f1$A			f2$B			f3$H			f4$J		

	#0	`A`:	f1@A[f1$A!11]
	#1	`B`:	f1@B[f1$A!11]	f2@B[f2$B!6]
	#2	`E`:	f1@E[f1$A!11]	f2@B[f2$B!6]	f3@H[f3$H!5]
	#3	`F`:	f1@E[f1$A!11]	f2@B[f2$B!6]	f3@H[f3$H!5]
	#4	`G`:	f1@A[f1$A!11]	f2@B[f2$B!6]	f3@H[f3$H!5]	f4@J[f4$J!4]
	#5	`C`:	f1@B[f1$A!11]	f2@B[f2$B!6]
	#6	`D`:	f1@B[f1$A!11]	f2@D[f2$B!6]
	#7	`H`:	f1@A[f1$A!11]					f3@H[f3$H!5]
	#8	`I`:	f1@A[f1$A!11]					f3@I[f3$H!5]	f4@J[f4$J!4]
	#9	`J`:	f1@A[f1$A!11]									f4@J[f4$J!4]
	#10	`K`:	f1@A[f1$A!11]									f4@K[f4$J!4]


*/
