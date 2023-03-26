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
