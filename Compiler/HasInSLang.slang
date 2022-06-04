// Python 235/14
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
	
// Slang 244(do-end)/226({})/21
unit Duck
	fly do
		IO.put("Duck flying")
	end
end

unit Sparrow
	fly do
		IO.put("Sparrow flying")
	end
end

unit Whale
	swim do
		IO.put("Whale swimming")
	end
end

while animal in (Duck, Sparrow, Whale) do
	animal.fly
end
// type of animal is Duck|Sparrow|Whale - no need to look for the neart common ancestor - find among types tops in terms fcomfrmnace and build of them multitype !!!

/*
unit Duck
	fly {
		IO.put("Duck flying")
	}
}

unit Sparrow
	fly {
		IO.put("Sparrow flying")
	}
}

unit Whale
	swim {
		IO.put("Whale swimming")
	}
}

while animal in (Duck, Sparrow, Whale) {
	animal.fly
}
*/