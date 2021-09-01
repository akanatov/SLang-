use TestSupport  

title is "inline if-then-else operator"
section is "conditon.operators" 
topic is "'then' branch executes if conditon is true"

// Arrange
flag is true

// Act
return if flag then 0 else 1