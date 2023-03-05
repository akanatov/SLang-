alias rtn (T1, T2) as X // Alias for the routine type
alias A | B | C  as Y 	// Alias for the multi-type type
alias (A, B, C) as Z, A as AliasedA // Alias for the tuple type

//alias Z as T			// New  unit  based on existing one

a00: X
a0: X is rtn (t1: T1; t2: T2) do end
a1: Y is new A
a2: Z is (new A, new B, new C)
a3: Z //is new Z

unit  Any end
unit  A end
unit  B end
unit  C end
unit  T1 end
unit  T2 end
