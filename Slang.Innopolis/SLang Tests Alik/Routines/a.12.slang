use TestSupport  

title is "preconditions in functions"
section is "contracts.functions" 
topic is "Precondition violation throws 'RequireViolation' exception"


random(a: Integer, b: Integer): Integer is
	require
		a_is_positive: a > 0,
		b_is_more_than_a: b > a
	
	return 4
end


try
	random(50, 10)	
catch(e: RequireViolation)
	return 1
end

return 0