abstract type () // That is tuple type

() (pos: Integer; value: Any)
	/// It puts element into tuple at position, put by position
require
	pos in 1 .. count /// Valid elelment position
foreign
	
() (fieldName: String; value: Any)
/// It puts element into field called fieldName, put by name
require
	fieldIndex(fieldName) > 0 /// Tuple must have this field
foreign

pure () (index: Integer): Any
/// Get tuple element at at postion index
require
	index in 1 .. count /// Valid index
foreign

pure () (fieldName: String): Any
/// Get tuple element by fieldName
require
	fieldIndex(fieldName) > 0 /// Tuple must have this field
foreign
	
pure count: Integer 
/// Return the number of elelemnts in the tuple
foreign
  
	{this} fieldIndex (fieldNAme: String): Integer
	foreign
	ensure
		return in 0 .. count /// If no such field then 0 otherwise valid position
	end // fieldIndex
  
require
	count >= 0 /// Tuple has 0 or more elements
end
