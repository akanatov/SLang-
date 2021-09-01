use TestSupport  

title is "if-then operator"
section is "conditon.operators" 
topic is "'then' branch executes if conditon is true"

// Arrange
flag is true

// Act
if flag then
	return 0
end

return 1