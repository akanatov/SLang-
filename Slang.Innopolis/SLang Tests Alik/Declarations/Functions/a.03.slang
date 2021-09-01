use TestSupport

title is "Declare function with parameters"
section is "declaration.functions-procedures"
topic is "Declared function with parameters should correctly perform" 

// Assert
double(a: Integer): Integer is
	return a * 2
end double

return double(4) = 8
