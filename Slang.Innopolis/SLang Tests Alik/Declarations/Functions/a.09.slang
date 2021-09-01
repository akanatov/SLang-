use TestSupport

title is "Declare safe function"
section is "declaration.functions-procedures"
topic is "Functions declared with 'safe' modifer should be valid" 

// Assert
safe sqr(a: Integer): Integer is
	return a * a
end