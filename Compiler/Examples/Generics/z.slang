a is A[Any]
type A
     A (any: Any) do end
end
type A[G] : A
      A do
            A(Any)   // Which constructor is called here?
      end
     A (any: Any) do end
end

type Any
end