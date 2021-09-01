use TestSupport

title is "a is new Type.init(some_value)"
section is "Declarations.Variables"
topic is "Variable should be initialized with value passed to the constructor" 

// act:
number is new Integer.init(4)

// assert:
return if number = 4 then 1 else 0