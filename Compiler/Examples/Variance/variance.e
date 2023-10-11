class X 
   foo (p: B)
      require
	     p.bar
	 end
end
class Y inherit X
   redefine foo (p:A)
end

class A
end
class B inherit A
  bar: Boolean
end

