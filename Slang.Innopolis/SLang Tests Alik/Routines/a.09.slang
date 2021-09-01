use TestSupport  

title is "Generic parameters in inline functions"
section is "generics.functions" 
topic is "Describe functions with generic parameters"


min[T](a: T, b: T) => if a < b then a else b end

return if addTo[5](6) = 11 then 1 else 0 end