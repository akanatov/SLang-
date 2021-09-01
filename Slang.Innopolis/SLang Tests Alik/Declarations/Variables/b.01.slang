use TestSupport

title is "Type mismatch in 'as' ... 'is'"
section is "Declarations.Variables"
topic is "Initial value should be of same type as variable following 'as' operator" 

// arrange:
expected is Integer.init(4)
 
// act:
a: as expected is 4.0