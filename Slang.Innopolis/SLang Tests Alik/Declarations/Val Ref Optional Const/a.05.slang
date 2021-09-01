use TestSupport

title is "Declare reference variables"
section is "declaration.(var-ref-optionals-const)"
topic is "Reference variable with same value should have different references"

// Arrange
b : ref Integer
c : ref Integer

// Assert
return b /= c
