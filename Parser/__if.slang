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

/*
if expr is
	:Type1
		statements1 
	:Type2
		statements2
end

if expr is
	Type1:
		statements1 
	Type2:
		statements2
end
*/