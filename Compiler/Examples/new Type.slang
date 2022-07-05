use rtn (T1, T2) as X 	// Named routine  unit 
use A | B | C  as Y 	// Named multi- unit 
use (A, B, C) as Z		// Named tuple
//use Z as T				// New  unit  based on existing one

a00: X

a0: X is rtn (t1: T1; t2: T2) do end

a1: Y is new A
a3: Z //is new Z

a2: Z is (new A, new B, new C)
  


 unit  Any end
 unit  A end
 unit  B end
 unit  C end
 unit  T1 end
 unit  T2 end
