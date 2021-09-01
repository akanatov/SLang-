use TestSupport  

title is "assigning tuples"
section is "declarations.tuples" 
topic is "tuple can not be assigned to another conformed tuple"

tupleA: (Integer, String, Boolean) 
tupleB: (Integer, String, Boolean) is (1, "str", true)


tupleA := tupleB