use TestSupport  

title is "loop invariant"
section is "loop.operators" 
topic is "Loop may have invariant section"

// Arrange
cnt = 0

// Act
while i in 1 .. 10 loop
	cnt := cnt + 1
	
	invariant
		true
end

