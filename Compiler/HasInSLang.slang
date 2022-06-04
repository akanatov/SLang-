// Python
/*
class Duck:
    def fly(self):
        print("Duck flying")

class Sparrow:
    def fly(self):
        print("Sparrow flying")

class Whale:
    def swim(self):
        print("Whale swimming")

for animal in Duck(), Sparrow(), Whale():
    animal.fly()
*/	
	
// Slang
unit Duck
	fly () do
        print("Duck flying")
	end
end

unit Sparrow
	fly () do
        print("Sparrow flying")
	end
end

unit Whale
	swim () do
        print("Whale swimming")
	end
end

while animal in (Duck(), Sparrow(), Whale()) do
	if animal is unit (rtn fly ()) do
		animal.fly ()
	end
end