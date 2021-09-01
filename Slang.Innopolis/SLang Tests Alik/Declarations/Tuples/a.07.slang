use TestSupport  

title is "assigning tuples"
section is "declarations.tuples" 
topic is "tuple can be assigned to another conformed tuple with using 'as' operator"


tupleB: (Integer, String, Boolean) is (1, "str", true)
tupleA: as tupleB

tupleA := tupleB