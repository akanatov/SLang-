use TestSupport  

title is "declare unit with constructor"
section is "unit.declarations" 
topic is "Unit with constructor should be compilable and constructor should be executed in object creation"


unit MyUnit
	a,b : Integer
	init(c, d: Integer) is
		a := c
		b := d
	end

end

m = MyUnit(2, 5)

return 1 if m.a = 2 and m.b = 5 else 0