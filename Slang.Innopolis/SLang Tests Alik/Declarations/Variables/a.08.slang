use TestSupport

title is "Short declaration"
section is "Declarations.Variables"
topic is "Declare variable and initialize with default constructor" 

// arrange:
expected is Integer.init()
 
// act:
a : Integer

// assert:
return areEqual(a, expected)