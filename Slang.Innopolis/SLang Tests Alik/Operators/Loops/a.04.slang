use TestSupport  

title is "loop variant"
section is "loop.operators" 
topic is "Loop may have variant section"

// Arrange
cnt = 0

// Act
while i in 1 .. 10 loop
	cnt := cnt + 1
	variant
		10 - i
end

