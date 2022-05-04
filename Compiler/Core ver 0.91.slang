// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.90 Oct 9th 2020

val unit Bit
    const
         0b0, 0b1
    end
	pure & alias and (other: as this): as this => if this = 0b0 do 0b0 elsif other = 0b0 do 0b0 else 0b1

	pure | alias or (other: as this): as this => if this = 0b1 do 0b1 elsif other = 0b1 do 0b1 else 0b0

	pure ^ alias xor (other: as this): as this => if this = other do 0b0 else 0b1
		
	pure ~ alias not (): as this => if this = 0b0 do 0b1 else 0b0
		
	pure + (other: as this): as this do
		/// Definition of addition
		if this = 0b0 do
			return other
		elsif other = 0b1 do
			raise "Bit overflow"
		else
			return 0b1
		end
	end // +
	pure - (other: as this): as this do
		/// Definition of subtraction 
		if this = other do
			return 0b0
		elsif this = 0b1 do
			return 0b1
		else
			raise "Bit underflow"
		end
	end // -
end

val unit Bit [N: Integer]
	const
	    {Bit.0b0 | Bit.0b1}N
	end
	{} data: val Array [0..N-1, Bit] // Bit field ...
	:= (other: Integer) do
	/// Set N low bits from Integer other into this
	// WIP!!!
		raise "Not implemented!!!"
		// WIP What if number of bits in Integer is less than N ?????
		
		while pos in 0.. N - 1 do
			this (pos) := other.data (pos)
		end
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
		value in 0..1 /// Valid value
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
		if (data (pos) = 0b0)
			return 0
		else
			return 1
		end
	ensure
		return in 0..1 /// Valid Integer from Bit 
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
		result is Bit [bitsCount] // Hmmm .... Dynamic bit field allocation
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

abstract unit Any use const Integer, Real, Boolean, Character, String, Bit [2 ** Integer.MaxInteger]
	// Adding all constant objects from basic units allows to use these constants without respective unit name prefix.


	/// Shallow equality tests
    = (that: ? as this ): Boolean foreign
	final /= (that: ? as this ): Boolean => not ( this = that)
    = (that: as this): Boolean foreign
	final /= (that: as this ): Boolean => not ( this = that)
	
	/// Deep equality tests
    == (that: ? as this ): Boolean foreign
	final /== (that: ? as this ): Boolean => not ( this == that)
    == (that: as this): Boolean foreign
	final /== (that: as this ): Boolean => not ( this == that)
	
	/// Assignment definition
	// {} := (that: ? as this ) foreign 
	/* Caveat !!! Check!!! 
		a1, a2: ? Type
		a1 := a2 // Valid !
		a1 := Type // Valid !
	*/
	{} := (that: as this ) foreign
	
	/// Utility
	toString: String foreign
	sizeof: Integer foreign
	
end // Any

final unit System
	clone (object: Any): as object foreign
		/// Shallow version of the object clone operation
	deepClone (object: Any): as object foreign
		/// Deep version of the object clone operation
end // System

final unit Platform
	BitsInByteCount is 8
	IntegerBitsCount is 32
	CardinalBitsCount is IntegerBitsCount
	CharacterBitsCount is 8
	BooleanBitsCount is 8
	PointerBitsCount is 32
	RealBitsCount is 64
	RealMantissaBitsCount is 55
	RealOrderBitsCount is 8
require
	RealBitsCount = RealMantissaBitsCount + RealOrderBitsCount + 1
end // Platform

abstract unit Numeric
	one: as this abstract
		/// Definition of One
	zero: as this abstract
		/// Definition of Zero
	+ (other: as this): as this abstract
		/// Definition of addition
	ensure
		this - other = old this
	end // +
	- (other: as this): as this abstract
		/// Definition of subtraction 
	ensure
		this + other = old this		
	end // -
	- : as this abstract
		/// Definition of negation
	ensure
		- old this = this
	end
	/ (other: as this): as this abstract
		/// Definition of division
	end
	* (other: as this): as this abstract
		/// Definition of multiplication
	end
	** (other: Numeric): as this abstract
		/// Definition of power
	end
require
	this = this * one	/// Multiplication constituent 1
	zero = this * zero	/// Multiplication constituent 2
	this = this + zero	/// Addition constituent 1
	one = one + zero	/// Addition constituent 2
	this = this ** one	/// Power constituent 1
	one = this ** zero	/// Power constituent 2
	zero = this - this	/// Subtraction constituent 1
	this = this - zero	/// Subtraction constituent 1
	one = this / this	/// Division constituent
end // Numeric

abstract unit Comparable 
	< (other: as this): Boolean abstract
	// It looks we may rely on default equality test from Any ...
	/*override = (other: as this): Boolean abstract
	end*/
	pure > (other: as this): Boolean => other < this
	pure <= (other: as this): Boolean => not (other < this)
	pure >= (other: as this): Boolean => not (this < other)
	pure max (other: as this): as this => if this > other do this else other
	pure min (other: as this): as this => if this < other do this else other
require
	not (this < this) /// Irreflexive comparison
end // Comparable

abstract unit Enumeration extend Comparable
	succ: as this abstract
	/// successor of the current object
	ensure
		return > this
	end
	pred: as this abstract
	/// predecessor of the current object
	ensure
		return < this
	end
	first: as this abstract
	/// first element of enumeration
	last: as this abstract
	/// last element of enumeration
	count: Integer abstract
	/// number of elements in the enumeration
	ord: Integer abstract
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

val unit Pointer
	/// Unit Pointer allows to introduce address arithmetics in a safe way :-)
	// WIP!!!
	override sizeof: Integer do
		return Platform.PointerBitsCount / Platform.BitsInByteCount
	end
	write (b: Byte) foreign
	write (ab: Array [Byte]) foreign
	read: Byte foreign
	read: Array [Byte] foreign
	{} var data : Bit [Platform.PointerBitsCount]	
	init (address: Integer) 
	require
		address >= 0 /// Pointer should be greater than zero
	do
		data := address.data
	end
	{} validPointer (p: Pointer): Boolean foreign
require
	validPointer (this)
end // Pointer

val unit Cardinal alias uInt extend Cardinal [Platform.CardinalBitsCount]
	init (value: as this) do
		data := value.data
	end
	init do
		/// Default init with zero
		data := Bit [Platform.CardinalBitsCount]
	end
end // Cardinal

val unit Cardinal alias uInt [BitsNumber: Cardinal] extend Numeric, Enumeration
	// WIP!!!!
	// the caveat how to distingush Integer.1 from Cardinal.1 - now Cardinal constants are not imported into Any deu to name clash!!! .... 
	// Need to think more ...
	// No need for cardinal so far ...
end

val unit Integer extend Integer [Platform.IntegerBitsCount]
	init (value: Integer) do
		data := value.data
	end
	init do
		/// Default init with zero
		data := Bit [Platform.IntegerBitsCount]
	end
end // Integer

val unit Integer [BitsNumber: Integer] extend Numeric, Enumeration
	minInteger is - (2 ** (BitsNumber - 1))
	maxInteger is 2 ** (BitsNumber - 1) - 1
	const /* That is ordered set defined as range of all Integer constant values (objects) */
		minInteger .. maxInteger
	end
	override one is 1
	override zero is 0
	override sizeof: Integer is BitsNumber / Platform.BitsInByteCount
	override pure succ: as this => this + one
	override pure pred: as this => this - one	
	override first is minInteger
	override last is maxInteger
	pure count: Integer => last - first + 1
	pure ord: Integer => this - minInteger + 1
	override + (other: as this): as this do
		// Bit-level implementation of Integer addition
		raise "Not implemented!!!"
	end
	+ (other: Real): as this do
		return this + other.toInteger
	end
	+ (other: Real): LongInteger do
		return this + other.toInteger
	end
	+ do
		// Unary plus
	end
	++ do
		this += one
	end
	+= (other: as this) do
		// Bit-level implementation of Integer increment
		raise "Not implemented!!!"
	end
	+= (other: Real) do
		this += other.toInteger
	end
	-- do
		this -= one
	end
	-= (other: as this) do
		// Bit-level implementation of Integer decrement
		raise "Not implemented!!!"
	end
	-= (other: Real) do
		this -= other.toInteger
	end
	override - (other: as this): as this do
		// Bit-level implementation of Integer subtraction
		raise "Not implemented!!!"
	end
	- (other: Real): as this do
		return this - other.toInteger
	end
	- do
		// Unary minus
		return 0 - this
	end
	override / (other: as this): as this do
		// Bit-level implementation of Integer division
		raise "Not implemented!!!"
	end
	\ (other: as this): as this do
		// Bit-level implementation of Integer division reminder
		raise "Not implemented!!!"
	end
	/ (other: as this): Real do
		// Bit-level implementation of precise division
		raise "Not implemented!!!"
	end
	/ (other: Real): Real do
		// Bit-level implementation of precise division
		raise "Not implemented!!!"
	end
	override * (other: as this): as this do
		// Bit-level implementation of Integer multiplication
		raise "Not implemented!!!"
	end
	* (other: Real): Real do
		return other * this
	end
	override ** (other: as this): as this do
		// Bit-level implementation of Integer rasing to power
		raise "Not implemented!!!"
	end
	override < (other: as this): Boolean do
		// Bit-level implementation of Integer less
		raise "Not implemented!!!"
	end
	override = (other: as this): Boolean do
		// Bit-level implementation of Inetegr equal
		return data = other.data
	end
	override := (other: val Integer) do
		init (other)
	end
	override := (other: ref Integer) do
		init (other)
	end
	:= (other: Real) do
		// Bit-level implementation of Real assignment
		raise "Not implemented!!!"
	end
	:= (other: Character) do
		// Bit-level implementation of Chracter assignment
	raise "Not implemented!!!"
		if data.sizeof = other.data.sizeof do
			data := other.data // Size of data is different!!!
		else
		end
	end
	pure toCharacter: Character
	require
		this in 0 .. 2 ** Platform.CharacterBitsCount /// containsChacraterCode: 
	do
		return Character.init (data.getLowBits (Platform.CharacterBitsCount))
	end
	init (value: Integer) do
		data := value.data
	end
	init do
		/// Default init
		data := Bit [BitsNumber]
	end
	{} var data: Bit [BitsNumber]	
require
	BitsNumber > 0 /// Number of bits in Integer must be greater than zero!
end // Integer

val unit Byte
	// WIP - to be fully implemented
	init (value: Byte) do
		data := value.data
	end
	init do
		/// Default init
		data := Bit [Platform.BitsInByteCount]
	end
	{} var data: Bit [Platform.BitsInByteCount]	
end

abstract unit Rational extend Numeric, Comparable
	var numerator: Integer
	var denominator: Integer
	pure toReal: Real => numerator / denominator
	// TBD!!!
require
	denominator /= 0 /// denominator must not be zero
end

abstract unit Fixed extend Numeric, Comparable
	var value: Integer
	var scaleFactor: Integer // if scaleFactor is negative do scale is 1/-scaleFactor otherwise scaleFactor is scaleFactor :-)
	pure toReal: Real => if scaleFactor < 0 do value / - scaleFactor else value * scaleFactor
	// TBD!!!
end

abstract unit Float [nBits: Integer] extend Numeric, Comparable
	toReal: Real abstract
	// TBD!!!
end

val unit Real extend Float [Platform.RealBitsCount]
	minReal: Real is -RealMaxMantissa * 10 ** RealMaxOrder
	maxReal: Real is RealMaxMantissa * 10 ** RealMaxOrder
	epsilon: Real is RealMinMantissa * 10 ** -RealMaxOrder
	{} RealMaxMantissa is 2 ** Platform.RealMantissaBitsCount - 1
	{} RealMinMantissa is 0.000001 /// TBD
	{} RealMaxOrder is 2 ** Platform.RealOderBitsCount - 1
	const
		{minReal + epsilon} <= maxReal
	end
	override one is 1.0
	override zero is 0.0
	override sizeof: Integer is Platform.RealBitsCount / Platform.BitsInByteCount
	override + (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	+ alias plus(other: Integer): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	+ alias plus do
		// Unary plus
	end
	override - (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	- alias minus (other: Integer): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	- alias minus do
		// Unary minus 
		// Bit-level implemenation
		raise "Not implemented!!!"
	end
	override / (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	/ (other: Integer): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override * (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	* (other: Integer): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override ** (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override ** (other: Integer): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override < (other: as this): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override < (other: Integer): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override = (other: as this): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override = (other: Integer): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override := (other: as this) do
		// Bit-level implementation
		data := other.data
	end
	:= (other: Integer) do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	toInteger: Integer do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	init (value: val Real) do
		data := value.data
	end
	init do
		data := Bit [Platform.RealBitsCount]
	end
	{} var data : Bit [Platform.RealBitsCount]	
end // Real

val unit Character alias Char extend Enumeration
	minCharacter is 0.ToCharacter
	maxCharacter is (2 ** Platform.CharacterBitsCount - 1).ToCharacter
	const // The set of all constant objects of unit type Character
		minCharacter .. maxCharacter
	end	
	override pure succ: as this => (this.toInteger + one).toCharacter
	override pure pred: as this => (this.toInteger - one).ToCharacter
	override first is minCharacter
	override last is maxCharacter
	count is 2 ** Platform.CharacterBitsCount
	ord: Integer => toInteger
	override < (other: as this): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override sizeof: Integer is Platform.CharacterBitsCount / Platform.BitsInByteCount
	pure toInteger: Integer do
		return Integer.init (data.ToInteger)
	ensure
		return in return.minInteger .. return.maxInteger
	end
	:= (other: Integer)
	require
		other in 0 .. 2 ** Platform.CharacterBitsCount - 1 /// Invalid character code		
	do
		data := other
	end
	init (value: val Character) do
		data := value.data
	end
	init do
		data := Bit [Platform.CharacterBitsCount]
	end
	{} var data : Bit [Platform.CharacterBitsCount]	
end // Character

val unit Boolean alias Bool extend Enumeration
	{this} one is 1
	{this} zero is 0
	const // That is ordered set of two constant object of type Boolean
		false.init (zero), true.init (one)		
	end
	
	override pure < (other: as this): Boolean => this.data < other.data
	override pure = (other: as this): Boolean => this.data = other.data
	override pure succ: as this => if this do false else true
	override pure pred: as this => if this do false else true
	override first is false
	override last is true
	count is 2
	pure ord: Integer => if this do one else zero 
	override sizeof: Integer is Platform.BooleanBitsCount / Platform.BitsInByteCount
	pure & alias and (other: as this): Boolean => if this do if other do true else false else false
		// 00 -> 0
		// 01 -> 0
		// 10 -> 0
		// 11 -> 1
	pure | alias or (other: as this): Boolean => if this = false do if other do true else false else true	
		// 00 -> 0
		// 01 -> 1
		// 10 -> 1
		// 11 -> 1
	pure && alias and then (other: as this): Boolean => if not this do false elsif not other do false else true
	pure || alias or else (other: as this): Boolean => if this do true elsif other do true else false
	pure ^ alias xor (other: as this): Boolean => if this do if other do false else true else if other do true else false		
		// 00 -> 0
		// 01 -> 1
		// 10 -> 1
		// 11 -> 0
	pure -> alias implies (other: as this): Boolean => not this or other
	pure ~ alias not : Boolean => if this do false else true
	pure toInteger: Integer => if this do one else zero
	init (value: val Boolean) do
		data := value.data
	end
	init do
		data := 0xb
	end
	{} init (value: Integer) 
	require	
		value in zero..one /// Valid Integer value
	do
		data := value
	end
	{} var data: Bit [Platform.BooleanBitsCount]	
require
	this and this = this /// idempotence of 'and'
	this or this = this /// idempotence of 'or'
	this and not this = false /// complementation
	this or not this = true /// complementation
end // Boolean

abstract unit AString extend Comparable
	/// Abstraction of String
end

unit String [N:Integer] extend AString, Array [Character, N]
	/// String with fixed length
	const
		{Character.minCharacter |.. Character.maxCharacter}N
		// "" .. "Character*"
	end
end

unit String extend AString
	const
		{Character.minCharacter |.. Character.maxCharacter}0+
		// "" .. "Character*"
	end
	/// Variable length String
	override < (other: as this): Boolean do
		if this = other do
			return false
		end
		// Check the content
		while pos in 1..count do
			// TODO WIP
			raise "Not implemented!!!"
		end
		return false
	end
	override = (other: as this): Boolean do
		if this.data = other.data do
			return true
		end
	raise "Not implemented!!!"
		// Check the content to code !!! WIP
		return true
	end
	
	pure count: Integer => data.count
	
	pure () (pos: Integer): Character
		/// Get character at position pos
	require 
		1 <= pos and then pos <= count /// Valid element index
	do
		return data (pos)
	end
	() (pos: Integer; value: G) 
		/// Set array element at position pos with value
	require 
		1 <= pos and then pos <= count /// Valid element index 
	do	
		data (pos) := value
	end
	() (from, to: Integer): as this
	// substring
	require	
		from <= to /// Valid charters' indices range
		1 <= from <= count ///
		1 <= to <= count ///
	do
		// WIP to implement
		raise "Not implemented!!!"
	end
	+ (other: as this): as this 
	do
		// WIP to implement
		raise "Not implemented!!!"
	ensure
		return.count = this.count + other.count /// Valid concatenated string length
	end
	init do
		data := Array [Character] (1, 0)
	end
	init (other: as this) do
		data := other.data
	end
	{} var data: Array [Character] // Dynamic empty array of characters
end // String

abstract unit AnArray [G]
	///   General one dimensional array  
	() (pos: Integer): G 
		/// Get array element at position pos
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index
		abstract
	end
	() (pos: Integer; value: G) 
		/// Set array element at position pos with value
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index 
		abstract
	end
	final count: Integer => upper – lower + 1 
		/// Number of elelments in the array
		
	lower: Integer abstract
		/// Lower index value
	upper: Integer abstract
		/// Upper index value
	() (from, to: Integer): as this  // get subarray
	require
		from <= to /// subarray not empty		
		lower <= from /// from is greater or equal than lower
		to <= upper /// to is less or equal than upper
	abstract
	ensure
		return.count = to - from + 1 /// resulting subarray has valid number of elements 
	end
	() (start: Integer; subArray: as this)  // put subarray
	require
		lower <= start and then start <= upper /// valid start position of subarray to be put
		start + subArray.count - 1 <= upper /// valid subarray length
	do
		while pos in start .. start + subArray.count - 1 do
			this (pos) := subArray (subArray.lower + pos - 1)
		end
	end
require
	count >= 0 /// Consistent array count must be greater than zero
	lower <= upper - 1 /// Consistent  array range – lower index is not greater than upper when array is not empty. Array is empty when lower = upper - 1
end // AnArray [G]

abstract unit OneDimentionalArray [G init ()] extend AnArray [G]
	override () (pos: Integer): G foreign
	override () (pos: Integer; value: G) foreign
	override pure () (from, to: Integer): Array [G]
	/// get subarray
	do 
		result is Array [G].init(from, to)
		while pos in from .. to do
			result (pos) := this (pos)
		end
		return result
	end
end

unit Array [G init (), N: Integer] extend OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a0 is Array [Integer, 5]
	// That is what we like to have ....
*/
	override lower: Integer is 1
	override upper: Integer is N

	init (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end

	init do
		element is G.init
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array [G extend Any init (), N: Integer]

unit Array [G init (), N: (Integer,Integer)] extend OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a1 is Array [Real, (10,25)]
	// That is what we like to have ....
*/
	override lower: Integer is N(1)
	override upper: Integer is N(2)
	init (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end
	init do
		element is G.init
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array [G extend Any init (), N: (Integer,Integer)]


unit Array [G init ()] extend OneDimentionalArray [G]
///   Dynamic array
// We can put info Array only objects which has constructor with empty signature !!! We are always safe - no uninitilized data!!! 
	override () (pos: Integer): G foreign
	override () (pos: Integer; value: G) foreign
	override var lower: Integer
	override var upper: Integer
	init 
		/// It creates an empty array, but lower index is set a 1
	do
		init (1, 0, G()) 
	end
	init (n: Integer; value: G) 
		/// It creates an array of n elements,  lower index is 1
	require
		n >= 0 
	do 
		init (1, n, value) 
	end
	init (n: Integer) 
	require
		n >= 0 
	do 
		init (1, n, G()) 
	end
	init (l, u: Integer) do
		init (l, u, G())
	end
	init (l, u: Integer; value: G) 
	require
		l <= u - 1 /// Consistent array borders, lower must be not greater than upper if array is not empty and 1 less than upper otherwise
	do
		lower := l; 
		upper := u; 
		fill (value); 
	end
	{} fill (value: G) do
		if lower <= upper do
			data := allocateArray (lower, upper, value.sizeof)
			while index in lower .. upper do  
				this (index) := value
			end
		end
	end
	{} var data: ?Pointer
	{} foreign allocateArray (l, u: Integer: size: Integer)
	require
		l <= u /// Consistent array borders
	end
end // Array [G extend Any init ()]

unit Array [G init (), dimentions: (Integer|Integer .. Integer)] extend Array [G, numberOfElementsInLinearArray ]
///   Multi dimentional static array .... WIP!!!
/*
	a0 is Array [Integer, (5, 4)]
	a1 is Array [Real, (1..4, 5..7, 3..7)]
	// That is what we like to have ....
*/
	//{} data: Array [G ......] /// Multi dimetional array is to be stored as one dimentional one!!! 
	{} numberOfElementsInLinearArray: Integer do
		var result is 0
		while dim in dimentions do
			if dim is 
				Integer:
					result := result + dim
				Integer .. Integer:
					result := result + dim.last - dim.first + 1
			else
				raise "Bad array"
			end
		end
		return result
	end
end // Array [G extend Any init (), dimentions: (Integer|Integer .. Integer)]

unit StandardIO 
	// Init procedure is missed or how to set the output is missed !!!!
	pure put (arguments: ()) do
		while argument in arguments do
			if argument is
				Integer:
					putInteger (argument)
				Character:
					putCharacter (argument)
				String:
					putString (argument)
				Real:
					putReal (argument)
				Boolean:
					putBoolean (argument)
				else
					putString (argument.ToString)
			end
		end
	end put
	{} putInteger (anInteger: Integer) foreign
	{} putCharacter (aCharacter: Character) foreign
	{} putString (aString: String) foreign
	{} putReal (aReal: Real) foreign
	{} putBoolean (aBoolean: Boolean) foreign
end // StandardIO

// How to deal with dynamic objects .... WIP !!!
unit DynamicObject
	/* This unit allows to deal with dynamically loaded/created objects 
	providing dynamic type checking.
	*/
	// Access members by name
	final call (rtn_name: String; args: ()) do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr is ProcedureDescriptor do
			call (rtn_descr, args)
		else
			raise "Call to non-existing procedure '" + rtn_name + "'"
		end			
	end
	final call (rtn_name: String; args: ()): Any do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr is FunctionDescriptor do
			return call (rtn_descr, args)
		else
			raise "Call to non-existing function '" + rtn_name + "'"
		end			
	end
	final get_value (attr_name: String): Any do
		attr_descr is findAttribute (attr_name)
		if attr_descr is AttributeDescriptor do
			return attr_descr.value
		else
			raise "Access to non-existing attribute '" + attr_name + "'"
		end
	end
	final set_value (attr_name: String; value: Any) do
		attr_descr is findAttribute (attr_name)
		if attr_descr is VariableDescriptor do
			attr_descr.value := value
		else
			raise "Access to non-existing/writable attribute '" + attr_name + "'"
		end
	end
	// Access members by object (address) 
	final routines() : Array [RoutineDecsriptor] foreign
	final attributes (): Array [AttributeDescriptor] foreign
	final call (rtn: ProcedureDecsriptor; args: ()) foreign
	final call (rtn: FunctionDecsriptor; args: ()): Any foreign
	final get_value (attr: AttributeDescriptor): Any => attr.value
	final set_value (attr: VariableDescriptor; value: Any) foreign
	final {} own findRoutine (rtn_name: String): ?RoutineDescriptor foreign
	final {} own findAttribute (attr_name: String): ?AttributeDescriptor foreign
	init do
		/// Create empty dynamic object
	end
	init (fileName: String) do
		/// Load dynamic object from file
		raise "Not implemented yet!!!"
	end
	init (bytes: Array [Byte]) foreign
		/// Create dynamic object from binary data sequence
	init (other: Any) foreign
		/// Create dynamic object from any other object
	
end

abstract unit MemberDescriptor
	final var name: String
end
abstract unit RoutineDescriptor extend MemberDescriptor
	final var arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final unit ProcedureDescriptor extend RoutineDescriptor
	() alias apply (args: ()) foreign
		// That is a procedure call
	init (aName: as name; args: as arguments) do
		name := aName
		arguments := args
	end
end // Procedure [Arguments -> ()]

final unit FunctionDescriptor extend RoutineDescriptor
	var type: TypeDescriptor
	() alias apply (args: Arguments): Result foreign
		// That is a function call
	init (aName: as name; args: as arguments; aType: as type) do
		name := aName
		arguments := args
		type := aType
	end
end // Function [Arguments -> (), Result]*/

abstract unit AttributeDescriptor extend MemberDescriptor
	var value: Any
	var type: TypeDescriptor
end // AttributeDescriptor

final unit VariableDescriptor  extend AttributeDescriptor 
	override := alias set(v: as value) do 
		value := v
	end
	init (aName: as name; aType: as type; aValue: as value) do
		name := aName
		type := aType
		value := aValue
	end
end // VariableDescriptor

final unit ConstantDescriptor  extend AttributeDescriptor 
	init (aName: as name; aType: as type; aValue: as value) do
		name := aName
		type := aType
		value := aValue
	end
end // ConstantDescriptor

final unit TypeDescriptior
	final var name: String
	final var routines : Array [RoutineDecsriptor]
	final var attributes : Array [AttributeDescriptor]
	final var parents: Array [TypeDesciptor]
	init (aName: as name; prtns: as parents; rtns: as routines; attrs: as attributes) do
		name := aName
		parents := prtns
		routines := rtns
		attributes := attrs
	end
end
