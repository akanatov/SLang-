val  type  Pointer
	override const sizeof: Integer is Platform.pointerBitsCount / Platform.bitsInByteCount
end