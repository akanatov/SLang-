use TestSupport

title is "Declare safe function"
section is "declaration.functions-procedures"
topic is "Functions declared with 'safe' modifer can read from variables declared in the outer scope" 

b is 4
// Assert
safe sqr(a: Integer): Integer is
	return a * b
end