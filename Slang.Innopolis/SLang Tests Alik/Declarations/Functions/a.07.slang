use TestSupport

title is "Declare procedure in short form"
section is "declaration.functions-procedures"
topic is "Functions declared in short form should perform correctly" 

// Assert
sqr(a: Integer) => a * a

return sqr(4) = 16