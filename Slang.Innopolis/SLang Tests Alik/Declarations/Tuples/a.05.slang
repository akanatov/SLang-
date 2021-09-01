use TestSupport  

title is "assigning elements by indexes"
section is "declarations.tuples" 
topic is "assign tuple element by index"


tuple is (1, "str", true)

tuple(1) := 2
tuple(2) := "str2"
tuple(3) := false

return if tuple(1) = 2
		and then tuple(2) = "str2"
		and then tuple(3) = false
		then 1 else 0 end 