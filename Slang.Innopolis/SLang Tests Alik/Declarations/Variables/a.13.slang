use TestSupport

title is "Multiple varibale declaration with 'is'"
section is "Declarations.Variables"
topic is "All variables declared with single 'is' operator should have same initial value" 

// act:
a, b, c is 4

// assert:
return areEqual(a, 4) and areEqual(b, 4) and areEqual(c, 4)