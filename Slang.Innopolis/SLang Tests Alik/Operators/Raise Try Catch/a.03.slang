use TestSupport  

title is "try-catch-else operator"
section is "try-catch.operators" 
topic is "Else branch should be executed if no catch satisfies type"


try
	raise 1 // I am not sure that it's valid
	return 1
catch (er: String)
	return 1
else 
	return 0
end

return 1