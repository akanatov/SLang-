use TestSupport  

title is "break operator"
section is "loop.operators" 
topic is "Stop iterating loop in break"

// Arrange
cnt = 0

// Act
while i in 1 .. 10 loop
	cnt := cnt + 1
	if i = 5 then break end
end

// Assert
return cnt = 5