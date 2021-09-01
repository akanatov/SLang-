use TestSupport  

title is "Generic constant parameters in inline functions"
section is "generics.functions" 
topic is "Describe functions with generic constant parameters"


addTo[N](b: Integer) => N + b

return if addTo[5](6) = 11 then 1 else 0