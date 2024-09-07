if expr is ? do
 	expr := expr
elseif expr is T1 do
 	expr := expr
elseif expr is
	:T1, T2, T3
		expr := expr
elseif expr  is
	:value
		expr := expr
	:value1 .. value2
		expr := expr
	:value3, value11 .. value22
		expr := expr
else
	expr := expr 
end

/*if argument is
	:Integer
		putInteger (argument)
	:Character
		putCharacter (argument)
	:String
		putString (argument)
	:T1, T2, T3
	:T4, T5, T6 .. T7
		do_action
		a := if a is
			:T1 1
			:T2 2
			else 3
		
	:T7
//	:T2 // CTE
	else
		putString (argument.toString)
end
*/
 type  T1 end
 type  T2 end
 type  T3 end
 type  T4 end
 type  T5 end
 type  T6 end
 type  T7 end


