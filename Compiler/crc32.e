class CRC32
feature
	hash_for_string (s: String): Integer is
	require
		non_null_string: s /= Void
	do
		Result := crc32string ($s, s.count)
	end -- hash_for_string
feature {None}
	crc32string (p: Pointer; len: Integer): Integer is
		external "C"
		alias "crc32string"
	end
end -- class CRC32
