	scanner (args: Array[String]) do
		if args.count = 0 do
			ScreenOutput.putNL ("Valid usage: scanner <file_name>")
		else
			scanner is SLang_scanner (args.item (1))
			if scanner.isReady do
				o is new ScreenAndFileOutput("_SLang_scanner.out")
				pool is SortedArray [String]
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
				o.close
				scanner.close
			else
				ScreenOutput.putNL ("File '" + args.item (1) + "' not found or cannot be opened")
			end // if
		end // if
	end 

