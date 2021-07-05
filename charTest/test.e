class Test
create
	init
feature
	init is
	local
		i, n: Integer
		ch: Character
	do
		print ("Character \n test%N")
		from
			i := 0
			n := 256
		until
			i > n
		loop
			ch.from_integer (i)
			print (ch)
			i := i + 1
		end
	end
end