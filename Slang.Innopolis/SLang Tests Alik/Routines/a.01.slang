use TestSupport  

title is "tuple as arguments"
section is "functions" 
topic is "pass named tuple as arguments"

// keep it until next version of language

args = (message: "hello", count: 3)

print(message: String, count: Integer) is
	for i in 1..count loop
		StandartIO.puts(message)
	end
end

print(args)