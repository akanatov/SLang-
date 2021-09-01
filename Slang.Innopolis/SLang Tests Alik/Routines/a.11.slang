use TestSupport  

title is "postconditions in functions"
section is "contracts.functions" 
topic is "Postconditions violation throws 'EnsureViolation' exception"


random(a: Integer, b: Integer): Integer is

	return 4

	ensure
		return in a..b
end


try
	random(5, 10)	
catch(e: EnsureViolation)
	return 1
end

return 0
