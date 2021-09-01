use TestSupport

title is "Declare function"
section is "declaration.functions-procedures"
topic is "Declared function should return value" 

// Assert
func(): Integer is
	return 4
end func

return func() = 4