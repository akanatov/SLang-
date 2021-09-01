use TestSupport  

title is "preconditions in functions"
section is "contracts.functions" 
topic is "Preconditions in function described after keyword `require`"


random(a: Integer, b: Integer): Integer is
	require
		a_is_positive: a > 0,
		b_is_more_than_a: b > a

		return (a + b) div 2
end

random(1, 2)