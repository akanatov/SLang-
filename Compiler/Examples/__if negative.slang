if argument 
	case  Integer 
		putInteger (argument)
	case  Character 
		putCharacter (argument)
	case  String 
		putString (argument)
	case  T1, T2, T3 
	case   T4, T5 
		do_action
		a := if a
			case  T1 1
			case  T2 2
			else 3
		
	else
		putString (argument.toString)
end

