use TestSupport

title is "Declare pure function"
section is "declaration.functions-procedures"
topic is "Functions declared with 'pure' modifer should be valid" 

// Assert
pure sqr(a: Integer): Integer is
	return a * a
end
