a is A
a.foo
a.goo

// Unit extentions are not supprted !!! Need to store IR in a different file !!!
extend unit A
	goo do end
end
unit A
	foo do end
end
unit Any
end
