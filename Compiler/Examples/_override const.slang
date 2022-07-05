val  unit  Pointer
	override const sizeof: Integer is Platform.pointerBitsCount / Platform.bitsInByteCount
end