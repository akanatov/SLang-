if argument 
	: Integer 
		putInteger (argument)
	: Character 
		putCharacter (argument)
	: String 
		putString (argument)
	: T1, T2, T3 
	:  T4, T5 
		do_action
		a := if a
			: T1 1
			: T2 2
			else 3
		
	else
		putString (argument.toString)
end

