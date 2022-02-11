a := a * b ^ c + 2
// a:= a.* (b.^ (c.+ (2)))
// a:= (a.* (b.^(c))).+ (2)
a := a |= c
a := not a
a := a and then v
a := 5 + 7 + this - return