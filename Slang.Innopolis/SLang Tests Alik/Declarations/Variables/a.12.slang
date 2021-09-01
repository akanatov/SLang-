use TestSupport

title is "'as' operator"
section is "Declarations.Variables"
topic is "Declaring variable with same type as another one" 

// arrange:
expected is Integer.init()
 
// act:
a: as expected 

// assert:
return areEqual(a, expected)