use TestSupport  

title is "Implicit type instantiation in generic functions"
section is "generics.functions" 
topic is "Implicit type instantiation in generic functions"


min[T](a: T, b: T) is
	return a < b then a else b
end

min(42, 12)