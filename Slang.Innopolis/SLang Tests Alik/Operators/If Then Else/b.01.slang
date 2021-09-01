use TestSupport  

title is "incorrect use of if-then operator"
section is "conditon.operators" 
topic is "condition is not closed by 'end' operator"

// Arrange
flag is true

// Act
if flag then
	return 0
// end

return 1