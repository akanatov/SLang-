use TestSupport  

title is "access to hidden final property"
section is "unit.access" 
topic is "hidden final property can not be accessed from derived container"


unit Base
	hidden final a,b : Integer
	init(c, d: Integer) is
		a := c
		b := d
	end

end

unit Extended extend Base 
	getA() => Base.a
end