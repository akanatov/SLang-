use TestSupport  

title is "while-loop as for operator"
section is "loop.operators" 
topic is "Iterate 'for' loop with 'i' in range"

// Arrange
cnt = 0

// Act
while i in 1 .. 10 loop
	cnt := cnt + 1
end

// Assert
return cnt = 10