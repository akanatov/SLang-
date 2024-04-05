return N + if N \ Platform.bitsInByteCount = 0 do 0 else 1
return N / platform + if N \ Platform.bitsInByteCount = 0 do 0 else 1
return N / Platform.bitsInByteCount + if N \ Platform.bitsInByteCount = 0 do 0 else 1
type N end // :-)

val type  Bit [n: Integer]
	override pure sizeof: Integer do
		// return sizeof in bytes !!!! N is a number of bits !!!
		return n / Platform.bitsInByteCount + if n \ Platform.bitsInByteCount = 0 do 0 else 1
	end
end
