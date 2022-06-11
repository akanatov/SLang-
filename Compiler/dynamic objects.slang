		do
			object is DynamicObject ("classfile.compiled")
			object.call ("foo", 1,2,3)
			result is object.call ("goo", true, false, "xyz")
			if result 
				case Integer do StandardIO.put ("Recieved Integer from loaded code ", result)
				else
					StandardIO.put ("Received nothing from loaded code")
			end
		when String do
			StandardIO.put ("Received exception from loaded code")
		end
