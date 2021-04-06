deferred class Output
feature {Any}

	frozen put (obj: Any) is
	require
		non_void_object: obj /= Void	
	local 
		aString: String
	do
		aString := obj.out
		lastLength := aString.count
		put_string (aString)
	end --  put

	frozen putNL (obj: Any) is
	require
		non_void_object: obj /= Void	
	do
		if lastLength > 0 then
			newLine
		end -- if
		put (obj)
		newLine
	end --  put

	frozen newLine is
	do
		lastLength := 0
		put_new_line
	end -- newLine
	
	frozen putLine (obj: Any) is
	require
		non_void_object: obj /= Void
	local	
		newLength: Integer
		aString: String
	do
		if lastLength > 0 then
			aString := obj.out
			newLength := aString.count
			reset_line
			put_string (aString)
			if lastLength - newLength > 0 then
				put_spaces (lastLength - newLength)
			end -- if
			lastLength := newLength
		else
			put (obj)
		end -- if
	end -- putLine

	frozen putInToLine (obj: Any) is
	require
		non_void_object: obj /= Void
	local	
		aString: String
	do
		if lastLength > 0 then
			aString := obj.out
			put_string (aString)
			lastLength := lastLength + aString.count
		else
			put (obj)
		end -- if
	end -- putInToLine

	
	frozen putArray (arr: Array [Any]) is
	require
		non_void_array: arr /= Void
	local
         i, n: INTEGER
	do
		from
			i := arr.lower
			n := arr.upper
		until
			i > n
		loop
			if arr.item(i) = Void then
				put_string ("N/A")
			else
				put_string (arr.item(i).out)
			end -- if
			if i < n then
				put_string (separator)
			end -- if
			i := i + 1
		end -- loop
	end -- putArray

	close is
	deferred
	end -- close
	
	setSeparator (s: String) is
	require
		separator_not_void: s /= Void
	do
		separator.copy (s)
	end -- setSeparator
	
feature {None}

	reset_line is
	deferred
	end -- 	reset_line
	
	put_new_line is
	deferred
	end -- put_new_line

	put_spaces (spacesToPad: Integer) is
	require
		spacesToPad_greater_than_zero: spacesToPad > 0
	deferred
	end -- put_spaces

	frozen lastLength: Integer
	frozen separator: String is ""
	
	put_string (s: STRING) is
	require
		non_void_string: s /= Void
	deferred
	end -- put_string
	
invariant
	separator_assigned: separator /= Void
	valid_length_of_last_string: lastLength >= 0
end -- class Output
class ScreenOutput
inherit
	Output
	end
feature {Any}
	close is
	do	
		-- Do nothing
	end -- close
feature {None}
	put_string (s: STRING) is
	do
		io.put_string (s)
	end -- put_string	  
	reset_line is
	do
		io.put_character ('%R')
	end -- 	eset_line
	put_spaces (spacesToPad: Integer) is
	local
		counter: Integer
	do
		from
			counter := spacesToPad
		until
			counter = 0
		loop
			io.put_character (' ')
			counter := counter - 1
		end -- loop
	end -- put_spaces	
	put_new_line is
	do
		io.put_character ('%N')
	end -- put_new_line
end -- class ScreenOutput
class FileOutput
inherit
	Output
	end
create
	init
feature {None}
	file: File
	init (fileName: String) is
	require
		file_name_not_void: fileName /= Void
	local	
		wasError: Boolean
	do
		if wasError then
			io.put_string ("Failed to create output file '")
			io.put_string (fileName)
			io.put_string ("'. All output will be to the screen only!!!%N")
			file := Void
		else
			create file.make_create_read_write (fileName)
		end -- if
	rescue
		if not wasError then
			wasError := True
			retry
		end -- if
	end -- init
	close is
	do
		if file /= Void then
			file.close
			file := Void
		end -- if
	end -- close
	put_string (s: STRING) is
	do
		if file /= Void then
			file.put_string (s)
		end -- if
	end -- put_string	  
	reset_line is
	do
		if file /= Void then
			-- Next line!
			file.put_character ('%N')
		end -- if
	end -- 	reset_line
	put_spaces (spacesToPad: Integer) is
	do
		-- Do nothing!
	end -- put_spaces	
	put_new_line is
	do
		if file /= Void then
			file.put_character ('%N')
		end -- if
	end -- put_new_line
end -- class FileOutput
class ScreenAndFileOutput
inherit
	ScreenOutput
		undefine
			close
		redefine
			put_string, reset_line, put_new_line
	end
	FileOutput
		undefine
			put_spaces
		redefine
			put_string, reset_line, put_new_line
	end
create
	init
feature {None}
	put_string (s: STRING) is
	do
		Precursor {ScreenOutput} (s)
		Precursor {FileOutput} (s)
	end -- put_string	  
	reset_line is
	do
		Precursor {ScreenOutput}
		Precursor {FileOutput}
	end -- 	eset_line
	
	put_new_line is
	do
		Precursor {ScreenOutput}
		Precursor {FileOutput}
	end -- put_new_line
end -- class ScreenAndFileOutput