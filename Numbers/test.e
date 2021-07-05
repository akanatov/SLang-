class Test
create
	make
feature {NONE}
	make (args: Array [String]) is
	local
		aString: String
	do
		if args = Void then
			print ("Please provide a command line parameter - number or boolean [or bit] ....%N")
		else
			aString := args.item (1)
			print ("String	: ") print (aString) print ("%N")
			if aString.is_integer then
				print ("Integer : ") print (aString.to_integer)  print ("%N")
			end
			if aString.is_real then
				print ("Real	: ") print (aString.to_real)  print ("%N")
			end
			if aString.is_double then
				print ("Double  : ") print (aString.to_double)  print ("%N")
			end
			if aString.is_boolean then
				print ("Boolean : ") print (aString.to_boolean)  print ("%N")
			end
		end
	end
end