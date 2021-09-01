// Core units to represent basic SLang types  (WIP - not all features are fully implemented!!! )
// Version 0.79 Sep 3rd 2019

val unit Bit
    const is
         0b0, 0b1
    end
	& alias and (other: as this): as this => if this = 0b0 then 0b0 elsif other = 0b0 then 0b0 else 0b1

	| alias or (other: as this): as this => if this = 0b1 then 0b1 elsif other = 0b1 then 0b1 else 0b0

	^ alias xor (other: as this): as this => if this = other then 0b0 else 0b1
		
	~ alias not : as this => if this = 0b0 then 0b1 else 0b0
		
	+ (other: as this): as this do
		/// Definition of addition
		if this = 0b0 do
			return other
		elsif other = 0b1 do
			raise "Bit overflow"
		else
			return 0b1
		end
	end // +
	- (other: as this): as this do
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