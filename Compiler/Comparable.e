deferred class SmartComparable
inherit
	Comparable
		redefine
			is_equal
	end
feature
	is_equal (other: like Current): Boolean is
	do
		Result := Current = other or else Current.same_type (other) and then sameAs (other)
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		if Current /= other then
			Result := Current.generating_type < other.generating_type
			if not Result and then Current.same_type (other) then 
				Result := lessThan (other)
			end -- if
		end -- if
	end -- infix "<"
feature {SmartComparable}
	sameAs (other: like Current): Boolean is
	require
		other_not_void: other /= Void
	deferred		
	end -- sameAs
	lessThan (other: like Current): Boolean is
	require
		other_not_void: other /= Void
	deferred
	end -- lessThan
end --  class SmartComparable