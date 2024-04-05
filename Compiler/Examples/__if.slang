if expr is ? do
 	expr := expr
elseif expr is T1 do
 	expr := expr
elseif expr  
	case T1, T2, T3
		expr := expr
elseif expr  	
	case value
		expr := expr
	case value1 .. value2
		expr := expr
	case value3, value11 .. value22
		expr := expr
else
	expr := expr 
end

/*if argument
	case Integer
		putInteger (argument)
	case Character
		putCharacter (argument)
	case String
		putString (argument)
	 case T1, T2, T3
	 case T4, T5, T6 .. T7
		do_action
		a := if a
			case T1 1
			case T2 2
			else 3
		
	case T7
//	case T2 // CTE
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


