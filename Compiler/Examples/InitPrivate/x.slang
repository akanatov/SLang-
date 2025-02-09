d is Derived

type Base
	{this}:
		attr1: T1
		attr2: T2
		attr3: T3
	{Any}:
	Base (any: Any) do
		attr1 is T1
		attr3 is T3 (any)
	end
	/*Base do
		attr1 is T1
		attr3 is T3 (Any)
	end*/
end
type Derived : Base
  /* 
	If Base has constructor with no parameters it will be called when Derived object created 
	Otherwise Derived must have at least one its own constructor where the first N statements should be calls to N parents constructors
  */
	Derived do
		Base (Any)
	end
end

type T1
	// No constructor with no parameters
end
type T2
	// Constructor with no parameters
	T2 do end
end
type T3
	// Constructor with parameters
	T3 (p: Any) do end
end
 

type Any end

