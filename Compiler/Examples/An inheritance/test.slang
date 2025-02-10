type A
	f1 do end
end
type B: A
	f3 do end
end
type C: A
	f2 do end
end
type D: B, C
end
type E: A
end
type F: A, E
end
type G: B, C
end
type Any end


a: A is G
a := F
a := D

