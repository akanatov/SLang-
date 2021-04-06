unit SL_Scanner
{}:
	init (args: Array[String])
	do
		o is new ScreenOutput
		if args.count = 0 do
			o.putNL ("Valid usage: scanner <file_name>")
		else
			scanner is new SLang_scanner.init (args.item (1))
			if scanner.isReady do
				o := new ScreenAndFileOutput("_SLang_scanner.out")
				pool is new Sorted_Array [String]. init
				scanner.setPool (pool)
				//scanner.enableComments
				scanner.nextToken
				o.putNL("R:C\tT'name\t\tT'string\tT'value")
				while scanner.token /= scanner.eof_token do
					o.putArray(
						scanner.tokenRow, ':', scanner.tokenCol, '\t', scanner.tokenName(scanner.token), '\t', scanner.tokenString, '\t', '\t',scanner.tokenValue					
					)
					o.newLine
					scanner.nextToken
				end // loop
				o.putArray(
					scanner.tokenRow, ':', scanner.tokenCol, '\t', scanner.tokenName(scanner.token), '\t', scanner.tokenString
				)
				o.newLine
				scanner.close
			else
				o.putNL ("File '" + args.item (1) + "' not found or cannot be opened")
			end // if
		end // if
		o.close
	end // init
end // unit SL_Scanner

