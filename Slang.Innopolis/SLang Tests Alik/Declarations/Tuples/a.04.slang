use TestSupport  

title is "accessing elements by indexes"
section is "declarations.tuples" 
topic is "get tuple element by index"


tuple is (1, "str", true)

return if tuple(1) = 1 
		and then tuple(2) = "str"
		and then tuple(3) = true
		then 1 else 0 end 