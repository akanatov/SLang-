class Test
create
	init
feature
	init is 
	local
		a: A [Any]
	do
		create {B [Test]}a
		print ("Generating type: " + a.generating_type + "%N")
		print ("Generator      : " + a.generator + "%N")
	end
end
class A [G]
end
class B [G] inherit A [G]
end
