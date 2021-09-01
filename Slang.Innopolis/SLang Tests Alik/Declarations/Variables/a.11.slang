use TestSupport

title is "Short declaration with 'is'"
section is "Declarations.Variables"
topic is "Declare variable and initialize with default constructor" 

// arrange:
expected is Integer.init()
 
// act:
a is Integer

// assert:
return areEqual(a, expected)