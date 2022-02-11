val unit Bit [N: Integer]
	const: // All Bit constants not longer than B bits in it
	    //{Bit.0b0 | Bit.0b1}N
	end
	{} data: val Array [0 .. N-1, Bit] // Bit field ...
	:= (other: Integer) do
	/// Set N low bits from Integer other into this
	// WIP!!!
		raise "Not implemented!!!"
		// WIP What if number of bits in Integer is less than N ?????
		
		/*while pos in 0 .. N - 1 do
			this (pos) := other.data (pos)
		end*/
	end
	override pure sizeof: Integer do
		// return sizeof in bytes !!!! N is a number of bits !!!
		return N / Platform.BitsInByteCount + if N \ Platform.BitsInByteCount = 0 do 0 else 1
	end
	pure & alias and (other: as this): as this do
		result is this
		while pos in 0 .. N - 1 do
			result (pos) := if this(pos) = 0 or else other(pos) = 0 do 0 else 1
		end
		return result
	end // and
	pure | alias or (other: as this): as this do
		result is this
		while pos in 0 .. N - 1 do
			result (pos) := if this(pos) = 1 or else other (pos) = 1 do 1 else 0
		end
		return result
	end // or
	pure ^ alias xor (other: as this): as this do
		result is this
		while pos in 0 .. N - 1 do
			result (pos) := if this (pos) = other(pos) do 0 else 1
		end
		return result
	end
	pure ~ alias not (): as this do
		result is this
		while pos in 0 .. N - 1 do
			result.toggle (pos)
		end
		return result
	end // not
	() (pos: Integer; value: Boolean)
	require
		pos in 0 .. N - 1 /// Valid bit position
	do	
		this (pos) := if value do 1 else 0
	end // ()
	() (pos: Integer; value: Integer)
	/// Set bit as position 'pos' to value 'value'
	require
		pos in 0 .. N - 1 /// Valid bit position
		value in 0 .. 1 /// Valid value
	do
		if value = 0 do
			data (pos) := 0b0
		else
			data (pos) := 0b1
		end
	end // ()
	pure () (pos: Integer): Boolean
	require
		pos in 0 .. N - 1 // Valid position
	do
		return this (pos) = 0b1
	end // ()
	pure () (pos: Integer): Integer
	/// Get bit value at position 'pos'
	require
		pos in 0 .. N - 1 /// Valid position	 
	do
		if (data (pos) = 0b0) do
			return 0
		else
			return 1
		end
	ensure
		return in 0 .. 1 /// Valid Integer from Bit 
	end // ()
	pure toInteger: Integer do
		/// Build Integer from Bit
		var result is 0
		var factor is 1
		while pos in 0 .. N - 1 do
			if this (pos) = 1 do
				result := result + factor
				factor := factor * 2
			end
		end
		return result
	end // ToInteger
	pure getLowBits (bitsCount: Integer) : Bit [bitsCount]
	require
		bitsCount in 1 .. N /// Valid Bits Count
	do
		result is new Bit [bitsCount] // Hmmm .... Dynamic bit field allocation
		while pos in 0 .. bitsCount - 1 do
			result (pos) := this (pos)
		end
		return result
	end // getLowBits
	toggle (pos: Integer)
	require
		pos in 0 .. N - 1 /// validPos
	do
		this (pos) := if this (pos) = 1 do 0 else 1
	ensure
		( old this )(pos) /= this (pos) /// bit Toggled
	end // toggle
	init do
		/// Every BIT field is initialized with 0s
		while pos in 0 .. N - 1 do
			this (pos) := 0b0
		end
	end // init
require
	N > 0 /// Number of Bits is greater than zero
end // Bit
