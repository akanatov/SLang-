type B
end

type A  // CTE: type A has no init procedure which initializes field
  field: B
end


type AA
	field: B
	init do
		field := new B
	end
end

a is new AA