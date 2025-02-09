c is C

abstract type Any 
	foo (p: as this): Any abstract
end 

type C
	override foo (p: as this): Any do end
end 
