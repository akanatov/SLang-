use TestSupport  

title is "if-then-else operator"
section is "conditon.operators" 
topic is "'else' branch executes if conditon is false"

// Arrange
flag is false

// Act
if flag then
	return 1
else
	return 0
end

return 1