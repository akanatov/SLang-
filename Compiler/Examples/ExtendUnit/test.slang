a is A
a.foo
a.goo

// Unit extentions are not supprted !!! Need to store IR in a different file !!!
extend type A
	goo do end
end
type A
	foo do end
end
type Any
end
