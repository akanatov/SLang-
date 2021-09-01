use TestSupport  

title is "tuple as return type"
section is "functions" 
topic is "return named tuple as result"


sumAndMult(a: Integer, b: Integer): (sum: Integer, mult: Integer) is
	return (sum: a + b, mult: a * b)
end

res = sumAndMult(4, 3)

return if res.sum = 7
		and res.mult = 12
		then 1 else 0