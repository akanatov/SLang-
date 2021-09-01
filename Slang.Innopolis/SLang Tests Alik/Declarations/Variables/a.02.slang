use TestSupport

title is "Short declaration form"
section is "Declarations.Variables"
topic is "Variable should be initialized with value passed to the constructor" 

// act:
number is Integer(4)

// assert:
return if number = 4 then 1 else 0