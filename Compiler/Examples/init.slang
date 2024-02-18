unit B
end

unit A  // CTE: unit A has no init procedure which initializes field
  field: B
end


unit AA
	field: B
	init do
		field := new B
	end
end

a is new AA