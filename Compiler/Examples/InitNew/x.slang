unit Any end
unit Base1 end
unit Base2 end
unit Derived1 extend Base1, Base2
	field: Integer
	init: Base1, Base2 do
	end
end
