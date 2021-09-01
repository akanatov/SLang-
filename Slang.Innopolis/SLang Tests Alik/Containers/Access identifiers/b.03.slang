use TestSupport  

title is "access to hidden final property"
section is "unit.access" 
topic is "hidden final property can not be accessed from outside of a container"


unit Base
	hidden final a,b : Integer
	init(c, d: Integer) is
		a := c
		b := d
	end

end

b is Base(2, 3)
// should fail here
a is b.a