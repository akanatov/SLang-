use TestSupport

title is "'as' with 'is'"
section is "Declarations.Variables"
topic is "Declaring new varibale with type as another initialized with certain value" 

// arrange:
expected is Integer.init(4)
 
// act:
a: as expected is 4

// assert:
return areEqual(a, expected)