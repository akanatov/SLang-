c is C

abstract unit Any 
	foo (p: as this): Any abstract
end 

unit C
	override foo (p: as this): Any do end
end 
