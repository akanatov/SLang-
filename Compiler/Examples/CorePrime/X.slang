// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.99.14 Janury 1st 2023
build Kernel from "." // Kernel library is to be built from all SLang files from the current folder
end

val type Bit
    const: // Bit is just 0 or 1
         0b0, 0b1
    end
	pure & alias and (other: Bit): as this => if this = 0b0 do 0b0 elsif other = 0b0 do 0b0 else 0b1

end // Bit

val type Bit [bitsCount: Cardinal]
	const : // All Bit constants not longer than B bits in it
	    //{Bit.0b0 | Bit.0b1}bitsCount
	end
	{} data: val Array [Bit, (0, bitsCount-1)] // Bit field ...
	pure getLowBits (bitsToGet: Cardinal) : Bit [bitsCount]
	do
		result is new Bit [bitsToGet] // Hmmm .... Dynamic bit field allocation
	end // getLowBits
end // Bit [bitsCount]

abstract type Any use const Integer, Real, Boolean, Character, String, Bit [2 ^ Cardinal.maxCardinal], Pointer
end // Any

final type Platform
end // Platform

abstract type Numeric
end // Numeric

abstract type Comparable 
end // Comparable

abstract type Enumeration
end // Enumeration

type Memory
end // Memory

val type Pointer
	write (ab: Array [Byte]) none
	safe read (numberOfBytes: Cardinal): Array [Byte] none
	{} data : Bit [Platform.pointerBitsCount]
end // Pointer

val type Region
end // Region

val type Cardinal alias Uint : Cardinal [Platform.cardinalBitsCount]
	new Cardinal[Platform.cardinalBitsCount]( as this ), Cardinal[Platform.cardinalBitsCount]
end // Cardinal

val type Cardinal alias Uint [bitsCount: Cardinal] : Numeric, Enumeration

	Cardinal (other: Integer [bitsCount])
	do
	end // Cardinal
end // Cardinal

val type Integer : Integer [Platform.integerBitsCount]
	new Integer [Platform.integerBitsCount] (Integer), Integer [Platform.integerBitsCount]
end // Integer

val type Integer [bitsCount: Cardinal] : Numeric, Enumeration
	override safe = (other: Cardinal[bitsCount]): Boolean none 
	:= (other: Cardinal[bitsCount]) none 
	{} data: Bit [bitsCount]	
	Integer do
		data is Bit [bitsCount]
	end
end // Integer

val type Byte
	Byte do
		/// Default zero value
		data is Bit [Platform.bitsInByteCount]
	end
	{} data: Bit [Platform.bitsInByteCount]	
end // Byte

abstract type Rational : Numeric, Comparable
end // Rational

abstract type Fixed : Numeric, Comparable
end // Fixed

abstract type Float [nBits: Integer] : Numeric, Comparable
end // Float

val type Real : Float [Platform.realBitsCount]
	Real do
		data is Bit [Platform.realBitsCount]
	end
	{} data : Bit [Platform.realBitsCount]	
end // Real

val type Character alias Char : Enumeration
	Character do
		data is Bit [Platform.characterBitsCount]
	end
	{} data : Bit [Platform.characterBitsCount]	
end // Character

val type Boolean alias Bool : Enumeration
	{} data: Bit [Platform.booleanBitsCount]	
end // Boolean


abstract type AString : Comparable
	{} data: Array [Bit [8]] // Byte array of data
end // type Astring

type String [stringLength:Integer] : AString, Array [Character, stringLength]
end

type String : AString
	String do
		data is new Array [Character] (1, 0)
	end
	{} data: Array [Character] // Dynamic empty array of characters
end // String

abstract type AnArray [G]
end // AnArray [G]

abstract type OneDimentionalArray [G new ()] : AnArray [G]
	OneDimentionalArray do 
		result is new Array [G](from, to)
	end
end

type Array [G new (), arrayLength: Cardinal] : OneDimentionalArray [G]
end // Array

type Array [G new (), arrayBounds: (Integer,Integer)] : OneDimentionalArray [G]
end // Array


type Array [G new ()] : OneDimentionalArray [G]
end // Array


type DynamicObject
	final routines() : Array [RoutineDescriptor] none
	final attributes (): Array [AttributeDescriptor] none
	DynamicObject (bytes: Array [Byte]) none
	
end // DynamicObject

abstract type MemberDescriptor
end // MemberDescriptor

abstract type RoutineDescriptor : MemberDescriptor
	final arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final type ProcedureDescriptor : RoutineDescriptor
end // Procedure [Arguments -> ()]

final type FunctionDescriptor : RoutineDescriptor
end // Function [Arguments -> (), Result]

abstract type AttributeDescriptor : MemberDescriptor
end // AttributeDescriptor

final type VariableDescriptor : AttributeDescriptor 
end // VariableDescriptor

final type ConstantDescriptor : AttributeDescriptor 
end // ConstantDescriptor

final type TypeDescriptor
	final routines : Array [RoutineDescriptor]
	final attributes : Array [AttributeDescriptor]
	final parents: Array [TypeDescriptor]
end // TypeDescriptor

final type ArgumentDescriptor
end // ArgumentDescriptor
