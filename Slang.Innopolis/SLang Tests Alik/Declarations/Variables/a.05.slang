use TestSupport

title is "Full declaration form"
section is "Declarations.Variables"
topic is "Variable should be initialized with value passed to the constructor" 

// act:
number: Integer is new Integer.init(4)

// assert:
return if number = 4 then 1 else 0