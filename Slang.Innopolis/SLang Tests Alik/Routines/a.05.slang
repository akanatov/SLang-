use TestSupport  

title is "Generic constant parameters in functions"
section is "generics.functions" 
topic is "Describe functions with generic constant parameters"


addTo[N](b: Integer): Integer is
	return N + b
end

return if addTo[5](6) = 11 then 1 else 0