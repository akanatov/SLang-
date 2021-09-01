use TestSupport

title is "Declare function with default parameters"
section is "declaration.functions-procedures"
topic is "Declared function with default parameters should correctly perform" 

// Assert
mult(a: Integer is 4, b is 2): Integer is
	return a * b
end mult

return mult() = 8 and mult(5) = 10 and mult(1, 2) = 2