use TestSupport

title is "Assigning nullable variables"
section is "declaration.(var-ref-optionals-const)"
topic is "Types declared with '?' modifier can be used in right part of expression if it's type have been checked" 

// Arrange
b : ?Integer
b := 2

// Assert
if b is Integer then
	return b = 2
else
	return 0
end