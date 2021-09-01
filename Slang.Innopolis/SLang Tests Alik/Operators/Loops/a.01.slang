use TestSupport  

title is "while-loop operator"
section is "loop.operators" 
topic is "Execute loop body while condition is true"

// Arrange
i = 0

// Act
while i < 10 loop
	i := i + 1
end

// Assert
return i = 10