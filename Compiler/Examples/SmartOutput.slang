use StandardIO as IO
abstract  type  Output

	final put (obj: Any)
	do
		aString is obj.toString
		lastLength := aString.count
		put_string (aString)
	end //  put

	final putNL (obj: Any)
	do
		if lastLength > 0 do
			newLine
		end // if
		put (obj)
		newLine
	end //  put

	final newLine
	do
		lastLength := 0
		put_new_line
	end // newLine
	
	final putLine (obj: Any)
	do
		if lastLength > 0 do
			aString is obj.toString
			newLength is aString.count
			reset_line
			put_string (aString)
			if lastLength - newLength > 0 do
				put_spaces (lastLength - newLength)
			end // if
			lastLength := newLength
		else
			put (obj)
		end // if
	end // putLine

	final putInToLine (obj: Any)
	do
		if lastLength > 0 do
			aString is obj.toString
			put_string (aString)
			lastLength := lastLength + aString.count
		else
			put (obj)
		end // if
	end // putInToLine
	
	final putArray (arr: Array [Any])
	do
		while i in arr.lower .. arr.upper do
			if arr.item(i) = Void do
				put_string ("N/A")
			else
				put_string (arr.item(i).toString)
			end // if
			if i < n do
				put_string (separator)
			end // if
		end // loop
	end // putArray

	close abstract
	
	setSeparator (s: String)
	do
		separator.copy (s)
	end // setSeparator
	
{}:
	reset_line abstract
	
	put_new_line abstract

	put_spaces (spacesToPad: Integer)
	require
		spacesToPad > 0 /// spaces to pad <= 0 
	abstract

	final lastLength is 0
	final separator is ""
	
	put_string (s: STRING) abstract
	
require
	lastLength >= 0 /// invalid length of last string
end //  type  Output
 type  ScreenOutput : Output

	close
	do	
		// Do nothing
	end // close
{}:
	put_string (s: STRING)
	do
		io.put_string (s)
	end // put_string	  
	reset_line
	do
		io.put_character ('\r')
	end // 	reset_line
	put_spaces (spacesToPad: Integer)
	do
		while counter in 1 .. spacesToPad do
			io.put_character (' ')
		end // loop
	end // put_spaces	
	put_new_line
	do
		io.put_character ('\n')
	end // put_new_line
end //  type  ScreenOutput
 type  FileOutput : Output
{}:
	file: ?File
	init (fileName: String)
	do
		do
			create file.make_create_read_write (fileName)
		when FaliedToCreatFile do 
			io.put_string ("Failed to create output file '")
			io.put_string (fileName)
			io.put_string ("'. All output will be to the screen only!!!\n")
			?file
		end // if
	end // init
	close
	do
		if file is File do
			file.close
			?file
		end // if
	end // close
	put_string (s: STRING)
	do
		if file is File do
			file.put_string (s)
		end // if
	end // put_string	  
	reset_line
	do
		// Next line!
		if file is File do
			file.put_character ('\n')
		end // if
	end // 	reset_line
	put_spaces (spacesToPad: Integer)
	do
		// Do nothing!
	end // put_spaces	
	put_new_line
	do
		if file is File do
			file.put_character ('\n')
		end // if
	end // put_new_line
end //  type  FileOutput
 type  ScreenAndFileOutput : ScreenOutput, FileOutput
	override ScreenOutput.put_spaces, FileOutput.close
	new FileOutput
{}:
	override put_string (s: STRING)
	do
		old {ScreenOutput} (s)
		old {FileOutput} (s)
	end // put_string	  
	override reset_line
	do
		old {ScreenOutput}
		old {FileOutput}
	end // 	eset_line
	
	override put_new_line
	do
		old {ScreenOutput}
		old {FileOutput}
	end // put_new_line
end //  type  ScreenAndFileOutput