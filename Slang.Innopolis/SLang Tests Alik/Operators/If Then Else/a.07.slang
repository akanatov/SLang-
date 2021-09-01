use TestSupport  

title is "if-then-else-if operator"
section is "conditon.operators" 
topic is "'then' branch executes if conditon is true"

// Arrange
num = 22

// Act
if num mod 3 = 0 then
	return 1
elsif num mod 2 = 0 then
	return 0
else
	return 1
end

return 1