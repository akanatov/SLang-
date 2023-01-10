// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.99.14 Janury 1st 2023
build Kernel from "." // Kernel library is to be built from all SLang files from the current folder
end

val unit Bit
    const: // Bit is just 0 or 1
         0b0, 0b1
    end
	pure & alias and (other: Bit): as this => if this = 0b0 do 0b0 elsif other = 0b0 do 0b0 else 0b1

end // Bit

val unit Bit [bitsCount: Cardinal]
	const : // All Bit constants not longer than B bits in it
	    //{Bit.0b0 | Bit.0b1}bitsCount
	end
	{} data: val Array [Bit, (0, bitsCount-1)] // Bit field ...
	pure getLowBits (bitsToGet: Cardinal) : Bit [bitsCount]
	do
		result is new Bit [bitsToGet] // Hmmm .... Dynamic bit field allocation
	end // getLowBits
end // Bit [bitsCount]

abstract unit Any use const Integer, Real, Boolean, Character, String, Bit [2 ^ Cardinal.maxCardinal], Pointer
end // Any

final unit Platform
end // Platform

abstract unit Numeric
end // Numeric

abstract unit Comparable 
end // Comparable

abstract unit Enumeration
end // Enumeration

unit Memory
end // Memory

val unit Pointer
	write (ab: Array [Byte]) none
	safe read (numberOfBytes: Cardinal): Array [Byte] none
	{} data : Bit [Platform.pointerBitsCount]
end // Pointer

val unit Region
end // Region

val unit Cardinal alias Uint extend Cardinal [Platform.cardinalBitsCount]
	new Cardinal[Platform.cardinalBitsCount]( as this ), Cardinal[Platform.cardinalBitsCount]
end // Cardinal

val unit Cardinal alias Uint [bitsCount: Cardinal] extend Numeric, Enumeration

	Cardinal (other: Integer [bitsCount])
	do
	end // Cardinal
end // Cardinal

val unit Integer extend Integer [Platform.integerBitsCount]
	new Integer [Platform.integerBitsCount] (Integer), Integer [Platform.integerBitsCount]
end // Integer

val unit Integer [bitsCount: Cardinal] extend Numeric, Enumeration
	override safe = (other: Cardinal[bitsCount]): Boolean none 
	:= (other: Cardinal[bitsCount]) none 
	{} data: Bit [bitsCount]	
	Integer do
		data is Bit [bitsCount]
	end
end // Integer

val unit Byte
	Byte do
		/// Default zero value
		data is Bit [Platform.bitsInByteCount]
	end
	{} data: Bit [Platform.bitsInByteCount]	
end // Byte

abstract unit Rational extend Numeric, Comparable
end // Rational

abstract unit Fixed extend Numeric, Comparable
end // Fixed

abstract unit Float [nBits: Integer] extend Numeric, Comparable
end // Float

val unit Real extend Float [Platform.realBitsCount]
	Real do
		data is Bit [Platform.realBitsCount]
	end
	{} data : Bit [Platform.realBitsCount]	
end // Real

val unit Character alias Char extend Enumeration
	Character do
		data is Bit [Platform.characterBitsCount]
	end
	{} data : Bit [Platform.characterBitsCount]	
end // Character

val unit Boolean alias Bool extend Enumeration
	{} data: Bit [Platform.booleanBitsCount]	
end // Boolean


abstract unit AString extend Comparable
	{} data: Array [Bit [8]] // Byte array of data
end // unit Astring

unit String [stringLength:Integer] extend AString, Array [Character, stringLength]
end

unit String extend AString
	String do
		data is new Array [Character] (1, 0)
	end
	{} data: Array [Character] // Dynamic empty array of characters
end // String

abstract unit AnArray [G]
end // AnArray [G]

abstract unit OneDimentionalArray [G new ()] extend AnArray [G]
	OneDimentionalArray do 
		result is new Array [G](from, to)
	end
end

unit Array [G new (), arrayLength: Cardinal] extend OneDimentionalArray [G]
end // Array

unit Array [G new (), arrayBounds: (Integer,Integer)] extend OneDimentionalArray [G]
end // Array


unit Array [G new ()] extend OneDimentionalArray [G]
end // Array


unit DynamicObject
	final routines() : Array [RoutineDescriptor] none
	final attributes (): Array [AttributeDescriptor] none
	DynamicObject (bytes: Array [Byte]) none
	
end // DynamicObject

abstract unit MemberDescriptor
end // MemberDescriptor

abstract unit RoutineDescriptor extend MemberDescriptor
	final arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final unit ProcedureDescriptor extend RoutineDescriptor
end // Procedure [Arguments -> ()]

final unit FunctionDescriptor extend RoutineDescriptor
end // Function [Arguments -> (), Result]

abstract unit AttributeDescriptor extend MemberDescriptor
end // AttributeDescriptor

final unit VariableDescriptor extend AttributeDescriptor 
end // VariableDescriptor

final unit ConstantDescriptor extend AttributeDescriptor 
end // ConstantDescriptor

final unit TypeDescriptor
	final routines : Array [RoutineDescriptor]
	final attributes : Array [AttributeDescriptor]
	final parents: Array [TypeDescriptor]
end // TypeDescriptor

final unit ArgumentDescriptor
end // ArgumentDescriptor
