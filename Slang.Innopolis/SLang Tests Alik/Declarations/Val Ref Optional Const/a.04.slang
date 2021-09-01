use TestSupport

title is "Declare value-type variables"
section is "declaration.(var-ref-optionals-const)"
topic is "Value-type variables with same value should be equal"

// Arrange
b : val String is "hello"
c : val String is "hello"

// Assert
return b = c