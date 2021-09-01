use TestSupport

title is "~ Operator"
section is "declaration.operators.integer"
topic is "~ Operator on integer should return result of bitwise NOT on operand" 

// 10101 = 21
// 11111 = 31
// 01010 = 10
return (~21) & 31 = 10


