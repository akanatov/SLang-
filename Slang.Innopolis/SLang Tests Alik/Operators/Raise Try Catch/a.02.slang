use TestSupport  

title is "try-catch operator"
section is "try-catch.operators" 
topic is "'catch' block should be executed on exception raise"


try
	num = -1
	arr is Array[Integer](5)
	arr(num) = -1
	return 1
catch (er: String)
	return 0
end

return 1