use TestSupport  

title is "if-case operator"
section is "conditon.operators" 
topic is "Execute 'else' branch"

// Arrange
a is "string mesage"

// Act
if a is
Boolean:
	return 1
Integer:
	return 1
else
	return 0
end