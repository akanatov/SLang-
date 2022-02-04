// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.99.05 Feb 3rd 2022
build Kernel:"." // Kernel librray is to be built from all SLang files from the current folder
end

val unit Bit
    const : // Bit is just 0 or 1
         0b0, 0b1
    end
	pure & alias and (other: Bit): as this => if this = 0b0 do 0b0 elsif other = 0b0 do 0b0 else 0b1

	pure | alias or (other: Bit): as this => if this = 0b1 do 0b1 elsif other = 0b1 do 0b1 else 0b0

	pure ^ alias xor (other: Bit): as this => if this = other do 0b0 else 0b1
		
	pure ~ alias not (): Bit => if this = 0b0 do 0b1 else 0b0
		
	pure + (other: Bit): Bit do
		/// Definition of addition
		if this = 0b0 do
			return other
		elsif other = 0b1 do
			raise "Bit overflow"
		else
			return 0b1
		end
	end // +
	pure - (other: Bit): Bit do
		/// Definition of subtraction 
		if this = other do
			return 0b0
		elsif this = 0b1 do
			return 0b1
		else
			raise "Bit underflow"
		end
	end // -
end // Bit

/*virtual unit () // That is a tuple unit
	
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
	/// Return the number of elelments in the tuple
	foreign
	ensure
		return >= 0
	end
  
	{ this } pure fieldIndex (fieldName: String): Integer
	foreign
	ensure
		return in 0 .. count /// If no such field then 0 otherwise valid position
	end // fieldIndex
  
require
	count >= 0 /// Tuple has 0 or more elements
end*/

val unit Bit [N: Integer]
	const : // All Bit constants not longer than B bits in it
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
	Bit do
		/// Every BIT field is initialized with 0s
		while pos in 0 .. N - 1 do
			this (pos) := 0b0
		end
	end // Bit
require
	N > 0 /// Number of Bits is greater than zero
end // Bit [N]

virtual unit Any use const Integer, Real, Boolean, Character, String, Bit [2 ^ Integer.MaxInteger]
	// Adding all constant objects from basic units allows to use these constants without respective unit name prefix.


	/// Shallow equality tests
    pure = (that: ? as this ): Boolean foreign
	final pure /= (that: ? as this ): Boolean => not ( this = that);
    = (that: as this): Boolean foreign 
	final pure /= (that: as this ): Boolean => not ( this = that);
	
	/// Deep equality tests
    pure == (that: ? as this ): Boolean foreign
	final pure != (that: ? as this ): Boolean => not ( this == that);
    pure == (that: as this): Boolean foreign
	final pure != (that: as this ): Boolean => not ( this == that)
	
	/// Assignment definition
	// {} := (that: ? as this ) foreign 
	/* Caveat !!! Check!!! 
		a1, a2: ? Type
		a1 := a2 // Valid !
		a1 := Type // Valid !
	*/
//	{} := (that: as this ) foreign // No need to define default :=
	
	/// Utility
	pure toString: String foreign
	pure sizeOf: Integer foreign
	
end // Any

final unit System
	pure clone (object: Any): as object foreign
		/// Shallow version of the object clone operation
	pure deepClone (object: Any): as object foreign
		/// Deep version of the object clone operation
end // System

final unit Platform
	const BitsInByteCount is 8
	const IntegerBitsCount is 32
	const CardinalBitsCount is IntegerBitsCount
	const CharacterBitsCount is 8
	const BooleanBitsCount is 8
	const PointerBitsCount is 32
	const RealBitsCount is 64
	const RealMantissaBitsCount is 55
	const RealOrderBitsCount is 8

/*	const
		BitsInByteCount is 8,
		IntegerBitsCount is 32,
		CardinalBitsCount is IntegerBitsCount,
		CharacterBitsCount is 8,
		BooleanBitsCount is 8,
		PointerBitsCount is 32,
		RealBitsCount is 64,
		RealMantissaBitsCount is 55,
		RealOrderBitsCount is 8
*/
require
	RealBitsCount = RealMantissaBitsCount + RealOrderBitsCount + 1
end // Platform

virtual unit Numeric
	pure one: as this virtual
		/// Definition of One
	pure zero: as this virtual
		/// Definition of Zero
	pure + (other: as this): as this virtual
		/// Definition of addition
	ensure
		this - other = old this
	end // +
	pure - (other: as this): as this virtual
		/// Definition of subtraction 
	ensure
		this + other = old this		
	end // -
	pure - : as this virtual
		/// Definition of negation
	ensure
		- old this = this
	end
	pure / (other: as this): as this virtual
		/// Definition of division
	pure * (other: as this): as this virtual
		/// Definition of multiplication
	pure ^ (other: Numeric): as this virtual
		/// Definition of power
require
	this = this * one	/// Multiplication constituent 1
	zero = this * zero	/// Multiplication constituent 2
	this = this + zero	/// Addition constituent 1
	one = one + zero	/// Addition constituent 2
	this = this ^ one	/// Power constituent 1
	one = this ^ zero	/// Power constituent 2
	zero = this - this	/// Subtraction constituent 1
	this = this - zero	/// Subtraction constituent 1
	one = this / this	/// Division constituent
end // Numeric

virtual unit Comparable 
	pure < (other: as this ): Boolean virtual
	override pure = (other: as this ): Boolean => not (this < other) and not (other < this)
	pure > (other: as this ): Boolean => other < this
	pure <= (other: as this ): Boolean => not (other < this)
	pure >= (other: as this ): Boolean => not (this < other)
	pure max (other: as this ): as this => if this > other do this else other
	pure min (other: as this ): as this => if this < other do this else other
	pure compare (other: as this): -1 .. 1 => if this = other do 1 elsif this < other do -1 else 1;
require
	not ( this < this ) /// Irreflexive comparison
end // Comparable

virtual unit Enumeration extend Comparable
	pure succ: as this 
	/// successor of the current object
	require
		not (this = last)
	virtual
	ensure
		return > this /// Successor of the current element is greater than current one
		this.ord = return.ord - 1 /// Order number of this is order number of its successor minus one
	end
	
	pure pred: as this 
	/// predecessor of the current object
	require	
		not (this = first)
	virtual
	ensure
		return < this /// Predecessor of the current element is less than current one
		this.ord = return.ord + 1 /// /// Order number of this is order number of its predecessor plus one
	end
	
	pure first: as this 
	/// first element of enumeration
	require
		count > 0
	virtual
	
	pure last: as this 
	/// last element of enumeration
	require
		count > 0
	virtual
	
	pure count: Integer 
	/// number of elements in the enumeration
	virtual
	
	pure ord: Integer 
	/// order number of the current object in the enumeration
	require
		count > 0
	virtual
	ensure
		return > 0	// Order number of any element is greater than 0
		count > 0 implies return <= count // Order number of any element not greater than numbe rof elements
		count > 0 implies return in 1 .. count		
	end
require 
	count >= 0 /// Number of elements is not negative
//	count > 0 implies pred < this and then this < succ
	count > 0 implies first <= last
	count > 0 implies first.ord = 1
	count > 0 implies last.ord = count
end


val unit Pointer
	/// Unit Pointer allows to introduce address arithmetics in a safe way :-)
	// WIP!!!
	override const sizeof: Integer is Platform.PointerBitsCount / Platform.BitsInByteCount
	write (b: Byte) foreign
	write (ab: Array [Byte]) foreign
	read: Byte foreign
	read: Array [Byte] foreign
	{} data : Bit [Platform.PointerBitsCount]
	Pointer (address: Integer) 
	require
		address >= 0 /// Pointer should be greater than zero
		validPointer (address) /// Pointer should be valid from OS point of view
	do
		data is address.data
	end
	{} validPointer (p: Pointer): Boolean foreign
require
	validPointer (this)
end // Pointer

val unit Cardinal alias uInt extend Cardinal [Platform.CardinalBitsCount]
	Cardinal (value: as this) do
		data is value.data
	end
	Cardinal do
		/// Default with zero
		data is Bit [Platform.CardinalBitsCount]
	end
end // Cardinal

val unit Cardinal alias uInt [BitsNumber: Cardinal] extend Numeric, Enumeration
	// WIP!!!!
	// the caveat how to distingush Integer.1 from Cardinal.1 - now Cardinal constants are not imported into Any due to name clash!!! .... 
	// Need to think more ...
	// No need for cardinal so far ... But count should return Cardinal not Integer ...
	// x is y.count
	// if x = 1 it means that both Cardinal & Integer should have function '=' with another integral type !!!!
/*	Cardinal (other: Integer [???]) do
	end */
end // Cardinal

val unit Integer extend Integer [Platform.IntegerBitsCount]
	Integer (value: Integer) do
		data is value.data
	end
	Integer do
		/// Default with zero
		data is Bit [Platform.IntegerBitsCount]
	end
end // Integer

val unit Integer [BitsNumber: Integer] extend Numeric, Enumeration
	const : // That is ordered set defined as range of all Integer constant values (objects)
		minInteger .. maxInteger
	end
	const minInteger is - (2 ^ (BitsNumber - 1))
	const maxInteger is 2 ^ (BitsNumber - 1) - 1
	override const one is 1
	override const zero is 0
	override const sizeof: Integer is BitsNumber / Platform.BitsInByteCount
	override pure succ: as this => this + one
	override pure pred: as this => this - one
	override const first is minInteger
	override const last is maxInteger
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
		this.+=(one)
	end
	+= (other: as this) do
		// Bit-level implementation of Integer increment
		raise "Not implemented!!!"
	end
	+= (other: Real) do
		this.+=(other.toInteger)
	end
	-- do
		this.-=(one)
	end
	-= (other: as this) do
		// Bit-level implementation of Integer decrement
		raise "Not implemented!!!"
	end
	-= (other: Real) do
		this.-=(other.toInteger)
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
	override ^ (other: as this): as this do
		// Bit-level implementation of Integer rasing to power
		raise "Not implemented!!!"
	end
	override < (other: as this): Boolean do
		// Bit-level implementation of Integer less
		raise "Not implemented!!!"
	end
	override = (other: as this): Boolean do
		// Bit-level implementation of Integer equal
		return data = other.data
	end
	override = (other: Cardinal[BitsNumber]): Boolean do
		// Bit-level implementation of Integer-Cardinal comparison
		raise "Not implemented!!!"
		//return data = other.data
	end
	override := (other: val Integer) do
		Integer (other)
	end
	override := (other: ref Integer) do
		Integer (other)
	end
	:= (other: Real) do
		// Bit-level implementation of Real assignment
		raise "Not implemented!!!"
	end
	:= (other: Character) do
		// Bit-level implementation of Chracter assignment
		raise "Not implemented!!!"
		/*if data.sizeof = other.data.sizeof do
			data := other.data // Size of data is different!!!
		else
		end*/
	end
	:= (other: Cardinal[BitsNumber]) do
		// Assign cardinal to integer
		raise "Not implemented!!!"
	end
	pure toCharacter: Character
	require
		this in 0 .. 2 ^ Platform.CharacterBitsCount /// containsChacraterCode: 
	do
		return new Character (data.getLowBits (Platform.CharacterBitsCount))
	end
	{} data: Bit [BitsNumber]	
	Integer (value: Integer) do
		data is value.data
	end
	Integer do
		/// Default zero value
		data is Bit [BitsNumber]
	end
require
	BitsNumber > 0 /// Number of bits in Integer must be greater than zero!
end // Integer


//val unit Byte extend Bit [Platform.BitsInByteCount] use const Bit [Platform.BitsInByteCount]
//end

val unit Byte
	// WIP - to be fully implemented
	Byte (value: Byte) do
		data is value.data
	end
	Byte do
		/// Default zero value
		data is Bit [Platform.BitsInByteCount]
	end
	{} data: Bit [Platform.BitsInByteCount]	
end // Byte

virtual unit Rational extend Numeric, Comparable
	numerator: Integer
	denominator: Integer
	pure toReal: Real => numerator / denominator
	// TBD!!!
require
	denominator /= 0 /// denominator must not be zero
end // Rational

virtual unit Fixed extend Numeric, Comparable
	value: Integer
	scaleFactor: Integer // if scaleFactor is negative do scale is 1/-scaleFactor otherwise scaleFactor is scaleFactor :-)
	pure toReal: Real => if scaleFactor < 0 do value / - scaleFactor else value * scaleFactor
	// TBD!!!
end // Fixed

virtual unit Float [nBits: Integer] extend Numeric, Comparable
	toReal: Real virtual
	// TBD!!!
end // Float

val unit Real extend Float [Platform.RealBitsCount]
	const : // All Real numbers 
		//{minReal + epsilon} <= maxReal
		minReal {+epsilon} .. maxReal
	end
	const minReal: Real is -RealMaxMantissa * 10 ^ RealMaxOrder
	const maxReal: Real is RealMaxMantissa * 10 ^ RealMaxOrder
	const epsilon: Real is RealMinMantissa * 10 ^ -RealMaxOrder
	{} const RealMaxMantissa is 2 ^ Platform.RealMantissaBitsCount - 1
	{} const RealMinMantissa is 0.000001 /// TBD
	{} const RealMaxOrder is 2 ^ Platform.RealOderBitsCount - 1
	override const one is 1.0
	override const zero is 0.0
	override const sizeof: Integer is Platform.RealBitsCount / Platform.BitsInByteCount
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
	override ^ (other: as this): as this do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override ^ (other: Integer): as this do
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
	Real (value: val Real) do
		data is value.data
	end
	Real do
		data is Bit [Platform.RealBitsCount]
	end
	{} data : Bit [Platform.RealBitsCount]	
end // Real

val unit Character alias Char extend Enumeration
	const : // The set of all constant objects of unit type Character
		minCharacter .. maxCharacter
	end	
	const minCharacter is 0.ToCharacter
	const maxCharacter is (2 ^ Platform.CharacterBitsCount - 1).ToCharacter
	override pure succ: as this => (this.toInteger + one).toCharacter
	override pure pred: as this => (this.toInteger - one).ToCharacter
	override const first is minCharacter
	override const last is maxCharacter
	const count is 2 ^ Platform.CharacterBitsCount
	ord: Integer => toInteger
	override < (other: as this): Boolean do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	override const sizeof: Integer is Platform.CharacterBitsCount / Platform.BitsInByteCount
	pure toInteger: Integer do
		return new Integer (data.ToInteger)
	ensure
		return in return.minInteger .. return.maxInteger
	end
	:= (other: Integer)
	require
		other in 0 .. 2 ^ Platform.CharacterBitsCount - 1 /// Invalid character code		
	do
		data := other
	end
	Character (value: val Character) do
		data is value.data
	end
	Character do
		data is Bit [Platform.CharacterBitsCount]
	end
	{} data : Bit [Platform.CharacterBitsCount]	
end // Character

val unit Boolean alias Bool extend Enumeration
	const : // That is an ordered set of two constant object of type Boolean
		false(zero), true(one)		
	end
	{ this } const one is 1
	{ this } const zero is 0
	
	override pure < (other: as this): Boolean => this.data < other.data
	override pure = (other: as this): Boolean => this.data = other.data
	override pure succ: as this => if this do false else true
	override pure pred: as this => if this do false else true
	override const first is false
	override const last is true
	const count is 2
	pure ord: Integer => if this do one else zero 
	override const sizeof: Integer is Platform.BooleanBitsCount / Platform.BitsInByteCount
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
	Boolean (value: val Boolean) do
		data is value.data
	end
	Boolean do
		data is 0xb
	end
	{} Boolean (value: Integer) 
	require	
		value in zero..one /// Valid Integer value
	do
		data is value
	end
	{} data: Bit [Platform.BooleanBitsCount]	
require
	this and this = this /// idempotence of 'and'
	this or this = this /// idempotence of 'or'
	this and not this = false /// complementation
	this or not this = true /// complementation
end // Boolean

virtual unit AString extend Comparable
	/// virtualization of String
	{} data: Array [Bit [8]]
end

unit String [N:Integer] extend AString, Array [Character, N]
	/// String with fixed length
	const : // "" .. "Character*" - all Strings of size not more than N
		//{Character.minCharacter |.. Character.maxCharacter}N		
	end
end

unit String extend AString
	/// Variable length String
	const : // "" .. "Character*" - all Strings are defined here
		//{Character.minCharacter |.. Character.maxCharacter}0+		
	end
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
		//return true
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
	String do
		data is new Array [Character] (1, 0)
	end
	String (other: as this) do
		data is other.data
	end
	//String (:= data)
	{} data: Array [Character] // Dynamic empty array of characters
end // String

virtual unit AnArray [G]
	///   General one dimensional array  
	() (pos: Integer): G 
		/// Get array element at position pos
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index
		virtual
	() (pos: Integer; value: G) 
		/// Set array element at position pos with value
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index 
		virtual
	final count: Integer => upper - lower + 1
		/// Number of elelments in the array
		
	lower: Integer virtual
		/// Lower index value
	upper: Integer virtual
		/// Upper index value
	() (from, to: Integer): as this  // get subarray
	require
		from <= to /// subarray not empty		
		lower <= from /// from is greater or equal than lower
		to <= upper /// to is less or equal than upper
	virtual
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

virtual unit OneDimentionalArray [G new ()] extend AnArray [G]
	override () (pos: Integer): G foreign
	override () (pos: Integer; value: G) foreign
	override pure () (from, to: Integer): Array [G]
	/// get subarray
	do 
		result is new Array [G](from, to)
		while pos in from .. to do
			result (pos) := this (pos)
		end
		return result
	end
end

unit Array [G new (), N: Integer] extend OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a0 is Array [Integer, 5]
	// That is what we like to have ....
*/
	override lower: Integer is 1
	override upper: Integer is N

	Array (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end

	Array do
		element is new G
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array

unit Array [G new (), N: (Integer,Integer)] extend OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a1 is Array [Real, (10,25)]
	// That is what we like to have ....
*/
	override lower: Integer is N(1)
	override upper: Integer is N(2)
	Array (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end
	Array do
		element is new G
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array


unit Array [G new ()] extend OneDimentionalArray [G]
///   Dynamic array
// We can put info Array only objects which has constructor with empty signature !!! We are always safe - no uninitilized data!!! 
	override () (pos: Integer): G foreign
	override () (pos: Integer; value: G) foreign
	override lower: Integer
	override upper: Integer
	Array
		/// It creates an empty array, but lower index is set a 1
	do
		Array (1, 0, G()) 
	end
	Array (n: Integer; value: G) 
		/// It creates an array of n elements,  lower index is 1
	require
		n >= 0 
	do 
		Array (1, n, value) 
	end
	Array (n: Integer) 
	require
		n >= 0 
	do 
		Array (1, n, G()) 
	end
	Array (l, u: Integer) do
		Array (l, u, G())
	end
	Array (l, u: Integer; value: G) 
	require
		l <= u - 1 /// Consistent array borders, lower must be not greater than upper if array is not empty and 1 less than upper otherwise
	do
		lower is l 
		upper is u 
		fill (value)
	end
	{} fill (value: G) do
		if lower <= upper do
			data := allocateArray (lower, upper, value.sizeof)
			while index in lower .. upper do  
				this (index) := value
			end
		end
	end
	{} data: ?Pointer
	{} allocateArray (l, u: Integer; size: Integer)
	require
		l <= u /// Consistent array borders
	foreign
end // Array

/*unit Array [G new (), dimentions: (Integer|Integer .. Integer)] extend Array [G, numberOfElementsInLinearArray ]
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
				:Integer:
					result := result + dim
				:Integer .. Integer:
					result := result + dim.last - dim.first + 1
			else
				raise "Bad array"
			end
		end
		return result
	end
end // Array 
*/
unit StandardIO alias io
	StandardIO do
		// Init procedure is missed or way how to setup IO !!!!
	end -- StandardIO
	pure get: Integer foreign
	pure get: Real foreign
	pure get: Boolean foreign
	pure get: Character foreign
	pure get: String foreign
	pure put (arguments: ()) do
		while argument in arguments do
			if argument is
				:Integer
					putInteger (argument)
				:Character
					putCharacter (argument)
				:String
					putString (argument)
				:Real
					putReal (argument)
				:Boolean
					putBoolean (argument)
				else
					putString (argument.ToString)
			end
		end
	end // put
	pure put (anInteger: Integer) foreign
	pure put (aCharacter: Character) foreign
	pure put (aString: String) foreign
	pure put (aReal: Real) foreign
	pure put (aBoolean: Boolean) foreign
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
	final call (rtnDsc: ProcedureDescriptor; args: ()) foreign
	final call (rtnDsc: FunctionDecsriptor; args: ()): Any foreign
	final get_value (attr: AttributeDescriptor): Any => attr.value
	final set_value (attr: VariableDescriptor; value: Any) foreign
	{ this } final findRoutine (rtn_name: String): ?RoutineDescriptor foreign
	{ this } final findAttribute (attr_name: String): ?AttributeDescriptor foreign
	DynamicObject do
		/// Create empty dynamic object
	end
	DynamicObject (fileName: String) do
		/// Load dynamic object from file
		raise "Not implemented yet!!!"
	end
	DynamicObject (bytes: Array [Byte]) foreign
		/// Create dynamic object from binary data sequence
	DynamicObject (other: Any) foreign
		/// Create dynamic object from any other object
	
end // DynamicObject

virtual unit MemberDescriptor
	final name: String
end // MemberDescriptor

virtual unit RoutineDescriptor extend MemberDescriptor
	final arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final unit ProcedureDescriptor extend RoutineDescriptor
	() alias apply (args: ()) foreign
		// That is a procedure call
	ProcedureDescriptor (aName: as name; args: as arguments) do
		name is aName
		arguments is args
	end
//	ProcedureDescriptor (:= name; := arguments)
end // Procedure [Arguments -> ()]

final unit FunctionDescriptor extend RoutineDescriptor
	type: TypeDescriptor
	() alias apply (args: Arguments): Any foreign
		// That is a function call
	FunctionDescriptor (aName: as name; args: as arguments; aType: as type) do
		name is aName
		arguments is args
		type is aType
	end
//	FunctionDescriptor (:= name; := arguments; := type)
end // Function [Arguments -> (), Result]

virtual unit AttributeDescriptor extend MemberDescriptor
	value: Any
	type: TypeDescriptor
end // AttributeDescriptor

final unit VariableDescriptor extend AttributeDescriptor 
	override := alias set (v: as value) do 
		value := v
	end
	VariableDescriptor (aName: as name; aType: as type; aValue: as value) do
		name is aName
		type is aType
		value is aValue
	end
//	VariableDescriptor (:= name; := type; := value)
end // VariableDescriptor

final unit ConstantDescriptor extend AttributeDescriptor 
	ConstantDescriptor (aName: as name; aType: as type; aValue: as value) do
		name is aName
		type is aType
		value is aValue
	end
//	ConstantDescriptor (:= name; := type; := value)
end // ConstantDescriptor

final unit TypeDescriptior
	final name: String
	final routines : Array [RoutineDescriptor]
	final attributes : Array [AttributeDescriptor]
	final parents: Array [TypeDesciptor]
	TypeDescriptior (aName: as name; prtns: as parents; rtns: as routines; attrs: as attributes) do
		name is aName
		parents is prtns
		routines is rtns
		attributes is attrs
	end
//	TypeDescriptior (:= name; := parents; := routines; := attributes)
end // TypeDescriptior
