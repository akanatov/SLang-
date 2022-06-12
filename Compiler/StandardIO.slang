unit StandardIO alias IO
	readInt: Integer foreign
	readReal: Real foreign
	readBoolean: Boolean foreign
	readChar: Char foreign
	readString: String foreign
	// Init procedure is missed or how to set the output is missed !!!!
	pure put (arguments: ()) do
		while argument in arguments do
			if argument 
				case Integer do
					putInteger (argument)
				case Character do
					putCharacter (argument)
				case String do
					putString (argument)
				case Real do
					putReal (argument)
				case Boolean do
					putBoolean (argument)
				else
					putString (argument.toString)
			end
		end
	end // put
	{}: // Implementation routines
	putInteger (anInteger: Integer) foreign
	putCharacter (aCharacter: Character) foreign
	putString (aString: String) foreign
	putReal (aReal: Real) foreign
	putBoolean (aBoolean: Boolean) foreign
end // StandardIO
