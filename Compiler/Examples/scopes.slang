unit Test
	foo do end
	attr: Test
	{} goo do end
	{} attr1: Test
	{this} hidden do end
	{this} attr2: Test
	{}:
		notExported do end
		attr3: Test
	{this}:
		hidden1 do end
		attr4: Test
	{Any}:
		exported do end
		attr5: Test
end