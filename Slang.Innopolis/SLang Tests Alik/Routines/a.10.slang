use TestSupport  

title is "postconditions in functions"
section is "contracts.functions" 
topic is "Postconditions in function described after keyword `ensure`"


random(a: Integer, b: Integer): Integer is
	require
		a_is_positive: a > 0,
		b_is_more_than_a: b > a

		return (a + b) div 2

	ensure
		return in a..b
end

random(1, 2)