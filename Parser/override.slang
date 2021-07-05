return N + if N \ Platform.BitsInByteCount = 0 do 0 else 1
return N / Platform + if N \ Platform.BitsInByteCount = 0 do 0 else 1
return N / Platform.BitsInByteCount + if N \ Platform.BitsInByteCount = 0 do 0 else 1

val unit Bit [N: Integer]
	override pure sizeof: Integer do
		// return sizeof in bytes !!!! N is a number of bits !!!
		return N / Platform.BitsInByteCount + if N \ Platform.BitsInByteCount = 0 do 0 else 1
	end
end
