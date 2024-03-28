// Naming convention
a is A
// 'a' is the name of an attribute (variable or constant) - always starts with the lower case character
// 'A' is the name of a unit - always starts with the upper case character
// 'new A' can be written as just 'A'
// 'A.foo' calling 'foo' from the module 'A', no need for () if there are no parameters for 'foo'.
// by default all local attributes are 'const', all unit attributes are 'var'
const a is new A() // all defaults made explicit


// Super call
type Base1
	method (p: Type) do
	end
end
type Base2
	method (p: Type) do
	end
end
type Derived extend Base
	override method do
		old {Base1} (Type) // Call the overridden version from parent type Base1
		old {Base2} (Type) // Call the overridden version from parent type Base2
	end
end

// Multiple overriding, covariant overridng for type routines

// Covariant overrding for type attributes

type Base
	attr: Base
	method do // Assign types list: (attr <- {Base})
		attr := Base
	end
	method (p: Base) do // Assign types list: attr <- {Base}
		attr := p
	end
end

type Derived extend Base
	override attr: Derived // As attr is covariantly overrideen then all type routines of Base which write into attr are to be overriden
	override method do // Assign types list: (attr <- {Derived})
		attr := Derived
	end

	override method (p: Base) do // Assign types list: (attr <- {Derived})
		old (p) // CTE as overriden attr of type Derived does not conforms to at least one type of attr assign types of Base.method(Base)
		attr := convert_Base_into_Derived (p)
	end
	override method (p: Derived) do // Assign types list: (attr <- {Derived})
		attr := p
	end

	{} convert_Base_into_Derived (base: Base): Derived do
			return Derived
		end
end

()
[]
<>
{}

||
\\
//
**