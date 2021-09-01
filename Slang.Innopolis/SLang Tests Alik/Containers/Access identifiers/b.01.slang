use TestSupport  

title is "access to hidden property"
section is "unit.access" 
topic is "hidden property can not be accessed from outside of a container"


unit MyUnit
	hidden a,b : Integer
	init(c, d: Integer) is
		a := c
		b := d
	end

end

m = MyUnit(2, 5)

b is m.a