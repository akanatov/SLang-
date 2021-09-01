use TestSupport  

title is "Generic with constraints in functions"
section is "generics.functions" 
topic is "Describe functions with generic parameters and constraint"

unit MyBaseUnit
	compute(): Integer is abstract
end

calculate[T -> MyBaseUnit](obj: T): Integer is
	return obj.compute()
end