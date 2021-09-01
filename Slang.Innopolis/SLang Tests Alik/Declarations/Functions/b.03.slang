use TestSupport

title is "Invalid declaration of pure function"
section is "declaration.functions-procedures"
topic is "Functions declared with 'pure' modifer should not access variables declared in the outer scope" 

b is 4
// Assert
pure sqr(a: Integer): Integer is
	return a * b
end
