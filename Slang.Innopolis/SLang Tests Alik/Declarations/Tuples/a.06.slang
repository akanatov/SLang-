use TestSupport  

title is "assigning elements by names"
section is "declarations.tuples" 
topic is "assign tuple elements by names"


tuple is (num: 1, name: "str", flag: true)

tuple.num := 2
tuple.name := "slang"
tuple.flag := false

return if tuple.num = 2 
		and then tuple.name = "slang"
		and then tuple.flag = false
		then 1 else 0 end  