abstract  type  Enumeration : Comparable
	pure succ: as this 
	/// successor of the current object
	require
		not (this = last)
	abstract
	ensure
		return > this /// Successor of the current element is greater than current one
		this.ord = return.ord - 1 /// Order number of this is order number of its successor minus one
	end
end