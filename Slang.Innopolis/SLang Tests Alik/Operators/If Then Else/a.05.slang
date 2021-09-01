use TestSupport  

title is "if-case operator"
section is "conditon.operators" 
topic is "Execute branch with correct type"

// Arrange
a is true

// Act
if a is
Boolean:
	return 0
Integer:
	return 1
end

return 1