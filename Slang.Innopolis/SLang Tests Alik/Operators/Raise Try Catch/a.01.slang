use TestSupport  

title is "raise operator"
section is "try-catch.operators" 
topic is "'raise' should throw exception and give control to 'catch' block"


try
	raise "my exception"
	return 1
catch (er: String)
	return 0
end

return 1