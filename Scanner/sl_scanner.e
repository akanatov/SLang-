class SL_Scanner
creation
	init
feature {None}
	init (args: Array[String]) is 
	local
		scanner: SLang_scanner
		pool: Sorted_Array [String]
		o: Output
	do
		create {ScreenOutput}o
		if args = Void then
			o.putNL ("Valid usage: scanner <file_name>")
		else
			create scanner.init (args.item (1))
			if scanner.isReady then
				from 
					create {ScreenAndFileOutput}o.init ("_SLang_scanner.out")
					create pool.make
					scanner.setPool (pool)
					--scanner.enableComments
					scanner.nextToken
					o.putNL("R:C%TT'name%T%TT'string%TT'value")
				until
					scanner.token = scanner.eof_token
				loop
					o.putArray(<<
						scanner.tokenRow, ':', scanner.tokenCol, '%T', scanner.tokenName(scanner.token), '%T', scanner.tokenString, '%T', '%T',scanner.tokenValue					
					>>)
					o.newLine
					scanner.nextToken
				end -- loop
				o.putArray(<<
					scanner.tokenRow, ':', scanner.tokenCol, '%T', scanner.tokenName(scanner.token), '%T', scanner.tokenString
				>>)
				o.newLine
				scanner.close
			else
				o.putNL ("File '" + args.item (1) + "' not found or cannot be opened")
			end -- if
		end -- if
		o.close
	end -- init
end -- class SL_Scanner

