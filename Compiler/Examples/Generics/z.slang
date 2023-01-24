a is A[Any]
unit A
     A (any: Any) do end
end
unit A[G] extend A
      A do
            A(Any)   // Which constructor is called here?
      end
     A (any: Any) do end
end

unit Any
end