// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.99.22 March 29th 2024
build Kernel from "." // Kernel library is to be built from all SLang files from the current folder
end

val type Bit
    const: // Bit is just 0 or 1
         0b0, 0b1
    end
	safe & alias and (other: Bit): as this => if this = 0b0 do 0b0 elsif other = 0b0 do 0b0 else 0b1

	safe | alias or (other: Bit): as this => if this = 0b1 do 0b1 elsif other = 0b1 do 0b1 else 0b0

	safe ^ alias xor (other: Bit): as this => if this = other do 0b0 else 0b1
		
	safe ~ alias not (): Bit => if this = 0b0 do 0b1 else 0b0
		
	safe + (other: Bit): Bit do
		/// Definition of addition
		if this = 0b0 do
			return other
		elsif other = 0b1 do
			raise "Bit overflow"
		else
			return 0b1
		end
	end // +
	safe - (other: Bit): Bit do
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

/*val type BinaryDigit
    const: // Bit is just 0 or 1
         0, 1
    end
end
val type OctalDigit
    const:
         0, 1, 2, 3, 4, 5, 6, 7 
    end
end
val type DecimalDigit
    const:
         0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    end
end
val type HeximalDigit
    const:
         0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
    end
end*/

abstract type () : Enumeration // That is a tuple type
	
	() (pos: Cardinal; value: Any)
	/// It puts an element 'value' into the tuple at position 'pos'
	require
		pos in 1 .. count /// Valid element position
	none
	
	() (fieldName: String; value: Any)
	/// It puts an element 'value' into a field called fieldName
	require
		fieldIndex(fieldName) > 0 /// Tuple must have this field
	none

	safe () (pos: Cardinal): Any
	/// Get tuple element at postion 'pos'
	require
		pos in 1 .. count /// Valid element postion 
	none

	safe () (fieldName: String): Any
	/// Get tuple element by fieldName
	require
		fieldIndex(fieldName) > 0 /// Tuple must have this field
	none
	
	pure count: Cardinal
	/// Return the number of elements in the tuple
	none
  
	{ this } pure fieldIndex (fieldName: String): Cardinal
	none
	ensure
		return in 0 .. count /// If no such field then 0 otherwise a valid position
	end // fieldIndex

	override safe succ: as this => this(this.ord + 1)
	
	override safe pred: as this => this(this.ord - 1)
	
	override safe first: as this => this(1)
	
	override safe last: as this  => this(count)
	
	safe ord: Cardinal do
		while pos in 1 .. count do
			if this = this(pos) do
				return pos
			end // if
		end // while
	end // ord

end /// type ()

val type Bit [bitsCount: Cardinal]
	const : // All Bit constants not longer than B bits in it
	    //{Bit.0b0 | Bit.0b1}bitsCount
	end
	{} data: val Array [Bit, (0, bitsCount-1)] // Bit field ...
	:= (other: Cardinal) none
	:= (other: Integer) none /*do
	/// Set bitsCount low bits from Integer other into this
	// WIP!!!
		raise "Not implemented!!!"
		// WIP What if number of bits in Integer is less than bitsCount ?????
		
		/*while pos in 0 .. bitsCount - 1 do
			this (pos) := other.data (pos)
		end*/ 
	end */
	override pure sizeof: Cardinal do
		// return sizeof in bytes !!!! bitsCount is a number of bits !!!
		return bitsCount / Platform.bitsInByteCount + if bitsCount \ Platform.bitsInByteCount = 0 do 0 else 1
	end
	safe & alias and (other: as this): as this do
		result is this
		while pos in 0 .. bitsCount - 1 do
			result (pos) := if this(pos) = 0 or else other(pos) = 0 do 0 else 1
		end
		return result
	end // and
	safe | alias or (other: as this): as this do
		result is this
		while pos in 0 .. bitsCount - 1 do
			result (pos) := if this(pos) = 1 or else other (pos) = 1 do 1 else 0
		end
		return result
	end // or
	safe ^ alias xor (other: as this): as this do
		result is this
		while pos in 0 .. bitsCount - 1 do
			result (pos) := if this (pos) = other(pos) do 0 else 1
		end
		return result
	end
	safe ~ alias not (): as this do
		result is this
		while pos in 0 .. bitsCount - 1 do
			result.toggle (pos)
		end
		return result
	end // not
	() (pos: Cardinal; value: Boolean)
	require
		pos in 0 .. bitsCount - 1 /// Valid bit position
	do	
		this (pos) := if value do 1 else 0
	end // ()
	() (pos: Cardinal; value: Integer)
	/// Set bit as position 'pos' to value 'value'
	require
		pos in 0 .. bitsCount - 1 /// Valid bit position
		value in 0 .. 1 /// Valid value
	do
		if value = 0 do
			data (pos) := 0b0
		else
			data (pos) := 0b1
		end
	end // ()
	safe () (pos: Cardinal): Boolean
	require
		pos in 0 .. bitsCount - 1 // Valid position
	do
		return this (pos) = 0b1
	end // ()
	safe () (pos: Cardinal): 0 .. 1
	/// Get bit value at position 'pos'
	require
		pos in 0 .. bitsCount - 1 /// Valid position	 
	do
		if (data (pos) = 0b0) do
			return 0
		else
			return 1
		end
	end // ()
	safe toInteger: Integer do
		/// Build Integer from Bit  - sign bit is not processed !!!!! Negative !!!
		var result is 0
		var factor is 1
		while pos in 0 .. bitsCount - 1 do
			if this (pos) = 1 do
				result := result + factor
				factor := factor * 2
			end
		end
		return result
	end // ToInteger
	safe toCardinal: Cardinal do
		/// Build Integer from Bit
		var result: Cardinal is 0
		var factor is 1
		while pos in 0 .. bitsCount - 1 do
			if this (pos) = 1 do
				result := result + factor
				factor := factor * 2
			end
		end
		return result
	end // toCardinal
	safe getLowBits (bitsToGet: Cardinal) : Bit [bitsCount]
	require
		bitsToGet in 1 .. bitsCount /// Valid Bits Count
	do
		result is new Bit [bitsToGet] // Hmmm .... Dynamic bit field allocation
		while pos in 0 .. bitsToGet - 1 do
			result (pos) := this (pos)
		end
		return result
	end // getLowBits
	toggle (pos: Cardinal)
	require
		pos in 0 .. bitsCount - 1 /// validPos
	do
		this (pos) := if this (pos) = 1 do 0 else 1
	ensure
		( old this )(pos) /= this (pos) /// bit Toggled
	end // toggle
	init do
		/// Every BIT field is initialized with 0s
		while pos in 0 .. bitsCount - 1 do
			this (pos) := 0b0
		end
	end // Bit
require
	bitsCount > 0 /// Number of Bits is greater than zero
end // Bit [bitsCount]

abstract type Any use const Integer, Real, Boolean, Character, String, Bit [2 ^ Cardinal.maxCardinal], Pointer
	// Adding all constant objects from basic units allows to use these constants without respective type name prefix.


	/// Shallow equality tests
    safe = (that: ? as this ): Boolean none
	final safe /= (that: ? as this ): Boolean => not ( this = that);
    = (that: as this): Boolean none
	final safe /= (that: as this ): Boolean => not ( this = that);
	
	/// Deep equality tests
    safe == (that: ? as this ): Boolean none
	final safe != (that: ? as this ): Boolean => not ( this == that);
    safe == (that: as this): Boolean none
	final safe != (that: as this ): Boolean => not ( this == that)
	
	/// Assignment definition
	// {} := (that: ? as this ) foreign 
	/* Caveat !!! Check!!! 
		a1, a2: ? Type
		a1 := a2 // Valid !
		a1 := Type // Valid !
	*/
//	{} := (that: as this ) foreign // No need to define default :=
	
	/// Utility
	safe toString: String none
	safe sizeOf: Cardinal none
	
end // Any

final type System
	pure clone (object: Any): as object none
		/// Shallow version of the object clone operation
	pure deepClone (object: Any): as object none
		/// Deep version of the object clone operation
end // System

final type Platform

	const
		bitsInByteCount is 8,
		integerBitsCount is 32,
		cardinalBitsCount is integerBitsCount,
		characterBitsCount is 8,
		booleanBitsCount is 8,
		pointerBitsCount is 32,
		realBitsCount is 64,
		realMantissaBitsCount is 55,
		realOrderBitsCount is 8

require
	realBitsCount = realMantissaBitsCount + realOrderBitsCount + 1
end // Platform

abstract type Numeric
	pure one: as this abstract
		/// Definition of One
	pure zero: as this abstract
		/// Definition of Zero
	safe + (other: as this): as this abstract
		/// Definition of addition
	ensure
		this - other = old this
	end // +
	safe - (other: as this): as this abstract
		/// Definition of subtraction 
	ensure
		this + other = old this		
	end // -
	safe - : as this abstract
		/// Definition of negation
	ensure
		- old this = this
	end
	safe / (other: as this): as this abstract
		/// Definition of division
	safe * (other: as this): as this abstract
		/// Definition of multiplication
	safe ^ (other: Numeric): as this abstract
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

abstract type Comparable 
	safe < (other: as this ): Boolean abstract
	override safe = (other: as this ): Boolean => not (this < other) and not (other < this)
	final safe > (other: as this ): Boolean => other < this
	final safe <= (other: as this ): Boolean => not (other < this)
	final safe >= (other: as this ): Boolean => not (this < other)
	final safe max (other: as this ): as this => if this > other do this else other
	final safe min (other: as this ): as this => if this < other do this else other
	final safe compare (other: as this): -1 .. 1 => if this = other do 1 elsif this < other do -1 else 1;
require
	not ( this < this ) /// Irreflexive comparison
end // Comparable

abstract type Enumeration
	safe succ: as this 
	/// successor of the current object
	require
		not (this = last)
	abstract
	ensure
		this.ord = return.ord - 1 /// Order number of this is order number of its successor minus one
	end
	
	safe pred: as this 
	/// predecessor of the current object
	require	
		not (this = first)
	abstract
	ensure
		this.ord = return.ord + 1 /// Order number of this is order number of its predecessor plus one
	end
	
	safe first: as this 
	/// first element of enumeration
	require
		count > 0
	abstract
	
	safe last: as this 
	/// last element of enumeration
	require
		count > 0
	abstract
	
	safe count: Cardinal
	/// number of elements in the enumeration
	abstract
	
	safe ord: Cardinal
	/// order number of the current object in the enumeration
	require
		count > 0
	abstract

require 
	count > 0 implies first.ord <= last.ord
	count > 0 implies first.ord = 1
	count > 0 implies last.ord = count
	count > 0 implies this.ord in 1 .. count
end // Enumeration

type Memory
	allocate (size: Cardinal): Pointer
	require
		size > 0 /// Chunk should be not empty
	none
	free (area: Pointer)
	require
		areaAllocated (area)
	none
	areaAllocated(area: Pointer): Boolean
	none
end // Memory

val type Pointer alias Ptr
	/// Unit Pointer allows to introduce address arithmetics in a safe way :-)
	// WIP!!!
	const: 
		null (Cardinal.0)
	end
	override const sizeof: Cardinal is Platform.pointerBitsCount / Platform.bitsInByteCount
	write (b: Byte) none
	write (ab: Array [Byte]) none
	safe read: Byte none
	safe read (numberOfBytes: Cardinal): Array [Byte]
	require
		numberOfBytes > 0 /// Number of bytes to read is greater than zero
	none
	{} data : Bit [Platform.pointerBitsCount]
	init (address: Cardinal) 
	require
		validPointer (address) /// Pointer should be valid from the OS point of view: allocated if it is heap
	do
		data is address.data
	end
	{} validPointer (p: Pointer): Boolean foreign
		/// OS specific test for pointer validity
require
	validPointer (this) /// Any pointer must be valid in terms of underlying OS to provide acess to data
end // Pointer

val type Region
	address: Pointer
	size: Cardinal
	safe isAllocated: Boolean => size > 0
	allocate (aSize: Cardinal) 
	do
		//address := malloc (aSize)
	end // allocate
	/*{} final malloc (aSize: Cardinal): ???
	require
		aSize > 0 /// size to allocate should be gerater than 0
	foreign
	end // malloc */
end // Region

val type Cardinal alias Uint : Cardinal [Platform.cardinalBitsCount]
	init do init Cardinal[Platform.cardinalBitsCount] end
	init (other: Integer [Platform.integerBitsCount])
	require
		other >= 0 /// Integer must be not less than 0 to be converted into Cardinal
	do init Cardinal[Platform.cardinalBitsCount](other) end
end // Cardinal

val type Cardinal alias Uint [bitsCount: Cardinal] : Numeric, Enumeration
	// WIP!!!!
	// the caveat how to distingush Integer.1 from Cardinal.1 - now Cardinal constants are not imported into Any due to name clash!!! .... 
	// Cardinal must have := (that: Integer) to ensure we may assign Integer to Cardinal with proper precondition!!! 
	// Need to think more ...
	// No need for cardinal so far ... But count should return Cardinal not Integer ...
	// x is y.count
	// if x = 1 it means that both Cardinal & Integer should have function '=' with another integral type !!!!
	// If one like to assign Cardinal constant which is greater than Integer.max then prefix Cardinal. must be used ! 
	const : // That is ordered set defined as range of all Cardinal constant values (objects)
		minCardinal .. maxCardinal   // Parser bug - to be fixed !!!
	end
	const minCardinal is Cardinal.0
	const maxCardinal is Cardinal.2 ^ (bitsCount)
	override const one is Cardinal.1
	override const zero is Cardinal.0
	override const sizeof: Cardinal is bitsCount / Platform.bitsInByteCount

	override safe succ: as this => this + one
	override safe pred: as this => this - one
	override const first is Cardinal.0
	override const last is maxCardinal
	override const count: Cardinal is maxCardinal
	override safe ord: Cardinal => this

	init do
		/// Default zero value
		data is Bit [Platform.cardinalBitsCount]
	end
	init (other: Integer [bitsCount])
	require
		other >= 0 /// Integer must be not less than 0 to be converted into Cardinal
	do
		data is other.data
	end // Cardinal
	:= (other: Integer [bitsCount])
	require
		other >= 0 /// Integer must be not less than 0 to be converted into Cardinal
	do
		data := other.data
	end // :=
end // Cardinal

val type Integer alias Int : Integer [Platform.integerBitsCount]
	init (value: Integer) do init Integer [Platform.integerBitsCount] (value) end
	init do init Integer [Platform.integerBitsCount] end
end // Integer

val type Integer alias Int [bitsCount: Cardinal] : Numeric, Enumeration
	const : // That is ordered set defined as range of all Integer constant values (objects)
		minInteger .. maxInteger
	end
	const minInteger is - (2 ^ (bitsCount - 1))
	const maxInteger is 2 ^ (bitsCount - 1) - 1
	override const one is 1
	override const zero is 0
	override const sizeof: Cardinal is bitsCount / Platform.bitsInByteCount

	override safe succ: as this => this + one
	override safe pred: as this => this - one
	override const first is minInteger
	override const last is maxInteger
	override const count: Cardinal is last - first + one
	override safe ord: Cardinal => this - minInteger + one

	override safe + (other: as this): as this none /*do
		// Bit-level implementation of Integer addition
		raise "Not implemented!!!"
	end */
	safe + (other: Real): as this do
		return this + other.toInteger
	end
	/*pure + (other: Real): LongInteger do
		return this + other.toInteger
	end */ // Not quite sure we need Long types
	pure + do
		// Unary plus
	end
	++ do
		this.+=(one)
	end
	+= (other: as this) none /*do
		// Bit-level implementation of Integer increment
		raise "Not implemented!!!"
	end */
	+= (other: Real) do
		this.+=(other.toInteger)
	end
	-- do
		this.-=(one)
	end
	-= (other: as this) none /*do
		// Bit-level implementation of Integer decrement
		raise "Not implemented!!!"
	end */
	-= (other: Real) do
		this.-=(other.toInteger)
	end
	override safe - (other: as this): as this none /*do
		// Bit-level implementation of Integer subtraction
		raise "Not implemented!!!"
	end */
	safe - (other: Real): as this do
		return this - other.toInteger
	end
	- do
		// Unary minus
		return 0 - this
	end
	override safe / (other: as this): as this none /* do
		// Bit-level implementation of Integer division
		raise "Not implemented!!!"
	end */
	safe \ (other: as this): as this none /* do
		// Bit-level implementation of Integer division reminder
		raise "Not implemented!!!"
	end */
	safe / (other: as this): Real none /* do
		// Bit-level implementation of precise division
		raise "Not implemented!!!"
	end */
	safe / (other: Real): Real none /* do
		// Bit-level implementation of precise division
		raise "Not implemented!!!"
	end */
	override safe * (other: as this): as this do
		// Bit-level implementation of Integer multiplication
		raise "Not implemented!!!"
	end
	safe * (other: Real): Real do
		return other * this
	end
	override safe ^ (other: as this): as this none /* do
		// Bit-level implementation of Integer rasing to power
		raise "Not implemented!!!"
	end */
	override safe < (other: as this): Boolean none /* do
		// Bit-level implementation of Integer less
		raise "Not implemented!!!"
	end */
	override safe = (other: as this): Boolean do
		// Bit-level implementation of Integer equal
		return data = other.data
	end
	override safe = (other: Cardinal[bitsCount]): Boolean none /* do
		// Bit-level implementation of Integer-Cardinal comparison
		raise "Not implemented!!!"
		//return data = other.data
	end */
	override := (other: Integer) do
		// Integer (other)
		data := other.data
	end
	:= (other: Real) none /* do
		// Bit-level implementation of Real assignment
		raise "Not implemented!!!"
	end */
	:= (other: Character) none /* do
		// Bit-level implementation of Chracter assignment
		raise "Not implemented!!!"
		/*if data.sizeof = other.data.sizeof do
			data := other.data // Size of data is different!!!
		else
		end*/
	end */
	:= (other: Cardinal) none /* do
		// Assign cardinal to integer
		raise "Not implemented!!!"
	end */
	safe := alias toCharacter: Character
	require
		this in 0 .. 2 ^ Platform.characterBitsCount /// containsChacraterCode: 
	do
		return new Character (data.getLowBits (Platform.characterBitsCount))
	end
	{} data: Bit [bitsCount]	
	init (value: Integer) do
		// Not that simple !!! Copy the bits we need and pad the gap with 0 bits!!!
		data is value.data
	end
	init do
		/// Default zero value
		data is Bit [bitsCount]
	end
end // Integer

//val type Byte : Bit [Platform.bitsInByteCount] use const Bit [Platform.bitsInByteCount]
//end
val type Byte
	// WIP - to be fully implemented
	init (value: Byte) do
		data is value.data
	end
	init do
		/// Default zero value
		data is Bit [Platform.bitsInByteCount]
	end
	{} data: Bit [Platform.bitsInByteCount]	
end // Byte

abstract type Rational : Numeric, Comparable
	numerator: Integer
	denominator: Integer
	safe toReal: Real => numerator / denominator
	// TBD!!!
require
	denominator /= 0 /// denominator must not be zero
end // Rational

abstract type Fixed : Numeric, Comparable
	value: Integer
	scaleFactor: Integer // if scaleFactor is negative do scale is 1/-scaleFactor otherwise scaleFactor is scaleFactor :-)
	safe toReal: Real => if scaleFactor < 0 do value / - scaleFactor else value * scaleFactor
	// TBD!!!
end // Fixed

abstract type Float [nBits: Integer] : Numeric, Comparable
	toReal: Real abstract
	// TBD!!!
end // Float

val type Real : Float [Platform.realBitsCount]
	const : // All Real numbers 
		//{minReal + epsilon} <= maxReal
		minReal {+epsilon} .. maxReal
	end
	const minReal: Real is -maxMantissa * 10 ^ maxOrder
	const maxReal: Real is maxMantissa * 10 ^ maxOrder
	const epsilon: Real is minMantissa * 10 ^ -maxOrder
	{} const maxMantissa is 2 ^ Platform.realMantissaBitsCount - 1
	{} const minMantissa is 0.000001 /// TBD
	{} const maxOrder is 2 ^ Platform.realOrderBitsCount - 1
	override const one is 1.0
	override const zero is 0.0
	override const sizeof: Integer is Platform.realBitsCount / Platform.bitsInByteCount
	override safe + (other: as this): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	safe + alias plus(other: Integer): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	pure + alias plus do
		// Unary plus
	end
	override safe - (other: as this): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	safe - alias minus (other: Integer): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	- alias minus none /* do
		// Unary minus 
		// Bit-level implemenation
		raise "Not implemented!!!"
	end */
	override safe / (other: as this): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	safe / (other: Integer): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe * (other: as this): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	safe * (other: Integer): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe ^ (other: as this): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe ^ (other: Integer): as this none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe < (other: as this): Boolean none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe < (other: Integer): Boolean none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe = (other: as this): Boolean none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override safe = (other: Integer): Boolean none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	override := (other: as this) do
		data := other.data
	end
	:= (other: Integer) none /* do
		// Bit-level implementation
		raise "Not implemented!!!"
	end */
	safe toInteger: Integer do
		// Bit-level implementation
		raise "Not implemented!!!"
	end
	init (value: val Real) do
		data is value.data
	end
	init do
		data is Bit [Platform.realBitsCount]
	end
	{} data : Bit [Platform.realBitsCount]	
end // Real

val type Character alias Char : Enumeration
	const : // The set of all constant objects of type type Character
		minCharacter .. maxCharacter
	end	// const
	const minCharacter is 0.toCharacter
	const maxCharacter is (2 ^ Platform.characterBitsCount - 1).toCharacter
	override safe succ: as this => (this.toInteger + one).toCharacter
	override safe pred: as this => (this.toInteger - one).toCharacter
	override const first is minCharacter
	override const last is maxCharacter
	const count is 2 ^ Platform.characterBitsCount
	ord: Cardinal => toCardinal
	override safe < (other: as this): Boolean => data < other.data
	override const sizeof: Integer is Platform.characterBitsCount / Platform.bitsInByteCount
	safe toCardinal: Cardinal => new Cardinal (data.toCardinal)
	:= (other: Cardinal)
	require
		other in 0 .. 2 ^ Platform.characterBitsCount - 1 /// Invalid character code		
	do
		data := other
	end
	init (value: val Character) do
		data is value.data
	end
	init do
		data is Bit [Platform.characterBitsCount]
	end
	{} data : Bit [Platform.characterBitsCount]	
end // Character

val type Boolean alias Bool : Enumeration
	const : // That is an ordered set of two constant object of type Boolean
		false(zero), true(one)		
	end
	{ this } const one is 1
	{ this } const zero is 0
	
	override safe < (other: as this): Boolean => this.data < other.data
	override safe = (other: as this): Boolean => this.data = other.data
	override safe succ: as this => if this do false else true
	override safe pred: as this => if this do false else true
	override const first is false
	override const last is true
	const count is 2
	safe ord: Integer => if this do one else zero 
	override const sizeof: Integer is Platform.booleanBitsCount / Platform.bitsInByteCount
	safe & alias and (other: as this): Boolean => if this do if other do true else false else false
		/* 	00 -> 0
			01 -> 0
			10 -> 0
			11 -> 1 */
	safe | alias or (other: as this): Boolean => if this = false do if other do true else false else true
		/*	00 -> 0
			01 -> 1
			10 -> 1
			11 -> 1 */
	safe && alias and then (other: as this): Boolean => if not this do false elsif not other do false else true
	safe || alias or else (other: as this): Boolean => if this do true elsif other do true else false
	safe ^ alias xor (other: as this): Boolean => if this do if other do false else true else if other do true else false
		/*	00 -> 0
			01 -> 1
			10 -> 1
			11 -> 0 */
	safe -> alias implies (other: as this): Boolean => not this or other
	safe ~ alias not : Boolean => if this do false else true
	safe := : Cardinal => if this do 1 else 0
	init (value: val Boolean) do
		data is value.data
	end
	init do
		data is 0b0
	end
	{} init (value: Cardinal) 
	require	
		value in zero .. one /// Valid Cardinal value to be assigned into Boolean
	do
		if value = zero do
			data is 0b0
		else
			data is 0b1
		end 
		// data is value
	end
	{} data: Bit [Platform.booleanBitsCount]	
require
	this and this = this /// idempotence of 'and'
	this or this = this /// idempotence of 'or'
	this and not this = false /// complementation
	this or not this = true /// complementation
end // Boolean

// UTF-8, Unicode etc - need to think more 

abstract type AString : Comparable
	/// virtualization of String
	{} data: Array [Bit [8]] // Byte array of data
end // type Astring

type String [stringLength:Integer] : AString, Array [Character, stringLength]
	/// String with fixed length
	const : // "" .. "Character*" - all Strings of size not more than stringLength
		//{Character.minCharacter |.. Character.maxCharacter}stringLength
	end
end

type String : AString
	/// Variable length String
	const : // "" .. "Character*" - all Strings are defined here
		//{Character.minCharacter |.. Character.maxCharacter}0+		
	end
	override safe < (other: as this): Boolean do
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
	override safe = (other: as this): Boolean do
		if this.data = other.data do
			return true
		end
	raise "Not implemented!!!"
		// Check the content to code !!! WIP
		//return true
	end
	
	safe count: Cardinal => data.count
	
	safe () (pos: Cardinal): Character
		/// Get character at position pos
	require 
		1 <= pos and then pos <= count /// Valid element index
	do
		return data (pos)
	end
	() (pos: Cardinal; value: Character) 
		/// Set array element at position pos with value
	require 
		1 <= pos and then pos <= count /// Valid element index 
	do	
		data (pos) := value
	end
	safe () (from, to: Cardinal): as this
	// substring
	require	
		from <= to /// Valid charters' indices range
		1 <= from <= count ///
		1 <= to <= count ///
	none
	/*do
		// WIP to implement
		raise "Not implemented!!!"
	end*/
	safe + (other: as this): as this none /*
	do
		// WIP to implement
		raise "Not implemented!!!" */
	ensure
		return.count = this.count + other.count /// Valid concatenated string length
	end
	init do
		data is new Array [Character] (1, 0)
	end
	init (other: as this) do
		data is other.data
	end
	//String (:= data)
	{} data: Array [Character] // Dynamic empty array of characters
end // String

abstract type AnArray [G]
	///   General one dimensional array  
	() (pos: Integer): G 
		/// Get array element at position pos
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index
		abstract
	() (pos: Integer; value: G) 
		/// Set array element at position pos with value
		require 
			count > 0 /// Array is not empty
			lower <= pos and then pos <= upper /// Valid element index 
		abstract
	final safe count: Cardinal => upper - lower + 1
		/// Number of elelments in the array
		
	lower: Integer abstract
		/// Lower index value
	upper: Integer abstract
		/// Upper index value
	safe () (from, to: Integer): as this  // get subarray
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
	{}: // Low-level implementation of array concept
	data: ?Pointer
	allocateArray (l, u: Integer; size: Integer)
	require
		l <= u /// Consistent array bounds
	none
	ensure
		data is Pointer // data was allocated
	end	
require
	// Consistent array index range
	count = 0 implies lower = upper - 1 // Empty array indexes invariant
	count > 0 implies lower <= upper // Non-empty array indexes invariant
	// Was: lower <= upper - 1 
end // AnArray [G]

abstract type OneDimentionalArray [G init] : AnArray [G]
	override safe () (pos: Integer): G none
	override () (pos: Integer; value: G) none
	override safe () (from, to: Integer): Array [G]
	/// get subarray
	do 
		result is new Array [G](from, to)
		while pos in from .. to do
			result (pos) := this (pos)
		end
		return result
	end
end

type Array [G init, arrayLength: Cardinal] : OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a0 is Array [Integer, 5]
	// That is what we like to have ....
*/
	override lower: Integer is 1
	override upper: Integer is arrayLength

	init (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end

	init do
		element is new G
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array

type Array [G init, arrayBounds: (Integer,Integer)] : OneDimentionalArray [G]
 ///   One dimentional static array .... WIP !!!
/*
	a1 is Array [Real, (10,25)]
	// That is what we like to have ....
*/
	override lower: Integer is arrayBounds(1)
	override upper: Integer is arrayBounds(2)
	init (other: as this) do
		while pos in other.lower .. other.upper do
			this (lower + pos - 1) := other (pos)
		end
	end
	init do
		element is new G
		while pos in lower .. upper do
			this (pos) := element
		end
	end
end // Array


type Array [G init] : OneDimentionalArray [G]
///   Dynamic array
// We can put info Array only objects which has constructor with empty signature !!! We are always safe - no uninitilized data!!! 
	override safe () (pos: Integer): G none
	override () (pos: Integer; value: G) none
	override lower: Integer
	override upper: Integer
	init do init(1, 0, G) end
		/// It creates an empty array, but lower index is set a 1
	init (n: Cardinal; value: G) do init (1, n, value) end
		/// It creates an array of n elements,  lower index is 1
	init (n: Cardinal) do init (1, n, G) end
	init (l, u: Integer) do init (l, u, G) end
	init (l, u: Integer; value: G) 
	require
		l <= u - 1 /// Consistent array bounds, lower must be not greater than upper if array is not empty and 1 less than upper otherwise
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
			end // while
		end // if
	end
end // Array

/*type Array [G init, dimentions: (Integer|Integer .. Integer)] : Array [G, numberOfElementsInLinearArray ]
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
				:Integer
					result := result + dim
				:Integer .. Integer
					result := result + dim.last - dim.first + 1
			else
				raise "Bad array"
			end
		end
		return result
	end
end // Array 
*/
type StandardIO alias IO
	init do
		// Init procedure implementation is missed or way how to setup IO !!!!
	end // StandardIO
	pure get: Integer none
	pure get: Real none
	pure get: Boolean none
	pure get: Character none
	pure get: String none
	pure put (arguments: ()) do
		while argument in arguments do
			if argument is
				:Integer
					putInteger (argument)
				:Cardinal
					putCardinal (argument)
				:Character
					putCharacter (argument)
				:String
					putString (argument)
				:Real
					putReal (argument)
				:Boolean
					putBoolean (argument)
				else
					putString (argument.toString)
			end
		end
	end // put
	pure putNL (arguments: ()) do
		put (arguments)
		putCharacter ('\n')
	end // putNL
	pure put (anInteger: Integer) none
	pure put (aCardinal: Cardinal) none
	pure put (aCharacter: Character) none
	pure put (aString: String) none
	pure put (aReal: Real) none
	pure put (aBoolean: Boolean) none
end // StandardIO

// How to deal with dynamic objects .... WIP !!!
type DynamicObject
	/* This type allows to deal with dynamically loaded/created objects 
	providing dynamic type checking.
	*/
	// Access members by name
	final call (rtn_name: String; args: ()) do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr is ProcedureDescriptor do
			call (rtn_descr, args)
		else
			raise "Call to non-existing procedure `" + rtn_name + "`"
		end			
	end
	final call (rtn_name: String; args: ()): Any do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr is FunctionDescriptor do
			return call (rtn_descr, args)
		else
			raise "Call to non-existing function `" + rtn_name + "`"
		end			
	end
	final get_value (attr_name: String): Any do
		attr_descr is findAttribute (attr_name)
		if attr_descr is AttributeDescriptor do
			return attr_descr.value
		else
			raise "Access to non-existing attribute `" + attr_name + "`"
		end
	end
	final set_value (attr_name: String; value: Any) do
		attr_descr is findAttribute (attr_name)
		if attr_descr is VariableDescriptor do
			attr_descr.value := value
		else
			raise "Access to non-existing/writable attribute `" + attr_name + "`"
		end
	end
	// Access members by object (address) 
	final routines : Array [RoutineDescriptor] none
	final attributes: Array [AttributeDescriptor] none
	final call (rtnDsc: ProcedureDescriptor; args: ()) none
	final call (rtnDsc: FunctionDescriptor; args: ()): Any none
	final get_value (attr: AttributeDescriptor): Any => attr.value
	final set_value (attr: VariableDescriptor; value: Any) none
	{ this } final findRoutine (rtn_name: String): ?RoutineDescriptor none
	{ this } final findAttribute (attr_name: String): ?AttributeDescriptor none
	init
		/// Create empty dynamic object
	none
	init (fileName: String) do
		/// Load dynamic object from file
		raise "Not implemented yet!!!"
	end
	init (bytes: Array [Byte]) none
		/// Create dynamic object from binary data sequence
	init (other: Any) none
		/// Create dynamic object from any other object
	
end // DynamicObject

abstract type MemberDescriptor
	final name: String
end // MemberDescriptor

abstract type RoutineDescriptor : MemberDescriptor
	final arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final type ProcedureDescriptor : RoutineDescriptor
	() alias apply (args: ()) foreign
		// That is a procedure call
	init (:= name; := arguments) none
end // Procedure [Arguments -> ()]

final type FunctionDescriptor : RoutineDescriptor
	fType: TypeDescriptor
	() alias apply (args: ()): Any none
		// That is a function call
	init (:= name; := arguments; := fType) none
end // Function [Arguments -> (), Result]

abstract type AttributeDescriptor : MemberDescriptor
	value: Any
	aType: TypeDescriptor
end // AttributeDescriptor

final type VariableDescriptor : AttributeDescriptor 
	override := alias set (:= value) none
	init (:= name; := aType; := value) none
end // VariableDescriptor

final type ConstantDescriptor : AttributeDescriptor 
	init (:= name; := aType; := value) none
end // ConstantDescriptor

final type TypeDescriptor
	final name: String
	final routines : Array [RoutineDescriptor]
	final attributes : Array [AttributeDescriptor]
	final parents: Array [TypeDescriptor]
	init (:= name; := parents; := routines; := attributes) none
end // TypeDescriptor

final type ArgumentDescriptor
	final name: String
	final aType: TypeDescriptor	
	init (:= name; := aType) none
end // ArgumentDescriptor
