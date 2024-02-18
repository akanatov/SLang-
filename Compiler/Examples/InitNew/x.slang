unit Any end
unit Base1 end
unit Base2 end
unit Derived extend Base1, Base2
	init do
		init Base1
		init Base2
		init
	end
end

d is Derived

/*unit A[G] extend G //A[G]
	init: G none
end*/
