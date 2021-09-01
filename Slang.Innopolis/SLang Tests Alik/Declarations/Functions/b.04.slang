use TestSupport

title is "Invalid declaration of safe function"
section is "declaration.functions-procedures"
topic is "Functions declared with 'pure' modifer should not write to variables declared in the outer scope" 

b is 4
// Assert
safe func(a: Integer): Integer is
	b := 2
	return a * b
end
