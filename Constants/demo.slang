unit A extend Enumeration
  enum
	a, b, c, d, e , f
  end
  + (step: Integer): as this
  require
	step > 0 /// steps must be greater than 0
  do
    return := succ (this)
	while i in 2 .. step do
		return := succ (result)
	end // loop
  end // +	
end // A

use const A
while x in a| + 2 .. f do
end


// Just to remind the interface of 
virtual unit Enumeration extend Comparable
	succ: as this virtual
	/// successor of the current object
	ensure
		return > this
	end
	pred: as this virtual
	/// predecessor of the current object
	ensure
		return < this
	end
	first: as this virtual
	/// first element of enumeration
	last: as this virtual
	/// last element of enumeration
	count: Integer virtual
	/// number of elements in the enumeration
	ord: Integer virtual
	/// order of the current object in the enumeration, order starts with f_i_r_s_t :-) 
	ensure
		return > 0		
	end
require 
	succ > pred
	count >= 0
	ord > pred.ord
	ord < succ.ord
end
