if argument is
	:Integer
		putInteger (argument)
	:Character
		putCharacter (argument)
	:String
		putString (argument)
	else
		putString (argument.ToString)
end

