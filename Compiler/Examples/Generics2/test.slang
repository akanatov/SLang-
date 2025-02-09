any is Any

//------------

t1 is T1 [Any]
type T1 [G1] : P1 [G1]
end
type P1 [H1]
end

//------------

t2 is T2 [any]
type T2 [g2: Any] : P2 [g2]
end
type P2 [h2: Any]
end

//------------

t3 is T3[rtn do end]
type T3[g3: rtn] : P3 [g3]
end
type P3 [h3: rtn]
end

//------------
type Any
end

