type Any end
type Base1 end
type Base2 end
type Derived : Base1, Base2
	init do
		init Base1
		init Base2
		init
	end
end

d is Derived

/*type A[G] : G //A[G]
	init: G none
end*/
