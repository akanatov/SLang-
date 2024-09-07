foo do
if a is A do // type of 'a' is 'A'
else
end

if a is
	:A
       // statements
	:B
       // statements
		IO.put ("B!")
	:C, D
       // statements
    :1 .. 2, "a" .. "z"
       // statements
else
       // statements
		IO.put ("ELSE!")
end



x is 
	if a is
		:A // expression
		:B // expression
		:C, D // expression
	    :1 .. 2, "a" .. "z" // expression
	else // expression

/*
x is 
  a is C do 5 else 6


if a is
	:true
		IO.put ("True")
	:false
		IO.put ("False")
end
*/

/*
if expr is Type 
if expr is ?
if expr is : 
*/


end
