use TestSupport

title is "Shortest declaration"
section is "Declarations.Variables"
topic is "Varible should initialized with implicit constructor call with value followed by 'is' operator" 

// act:
number is 4

// assert:
return if number = 4 then 1 else 0