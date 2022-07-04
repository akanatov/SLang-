type TokenDescriptor
	token: Integer // current token
	tokenRow: Integer // current token row
	tokenCol: Integer // current token column
	buffer: String
	TokenDescriptor (t, r, c: Integer; b: String)
	do
		token:= t
		tokenRow:= r
		tokenCol:= c
		buffer:= b
	end // init
end // type TokenDescriptor

type SLang_Scanner
	token: Integer // current token
	tokenRow: Integer // current token row
	tokenCol: Integer // current token column

	tokenString: String // current token string representation
	do
		return pool.add_it (buffer)
		// return buffer
	end // tokenString
	
	tokenValue: ?Any // current token value, Void if not manifest constant
	do
		if token
			case integer_const_token
				return integer_value
			case real_const_token
				return buffer.to_real
			case string_const_token
				return buffer 
			case char_const_token
				return buffer (1)
			else
		end // if		
	ensure
		(return is Any) implies (
			token = integer_const_token or else 
			token = real_const_token or else
			token = string_const_token or else
			token = char_const_token
		)
	end // tokenValue
	
	sourceFileName: String
	
	tokenName (aToken: Integer): String 
	require
		aToken in 1 .. keywords.count // valid_token
	do
		return keywords(aToken)
	end // tokenName

	push 
	do
		stack.force ( TokenDescriptor (token, tokenRow, tokenCol, buffer), stack.count + 1)
	end // if
	
	revert
	do
		sp := 1
		nextToken		
	end // revert
	
	flush
	do
		stack.resize (1, 0)
		sp := 0
	end // if

	blockStart: Boolean => styleC and then token = left_curly_bracket_token or else token = do_token 
	
	blockEnd: Boolean => styleC and then token = right_curly_bracket_token	or else token = end_token 

	visibilityStart: Boolean => styleC and then token = left_square_bracket_token or else token = left_curly_bracket_token

	visibilityEnd: Boolean => styleC and then token = right_square_bracket_token or else token = right_curly_bracket_token
	
	genericsStart: Boolean => styleC and then token = less_token or else token = left_square_bracket_token

	genericsEnd: Boolean => styleC and then token = greater_token or else token = right_square_bracket_token


{}:
	pool: Sorted_Array [String]
	stack: Array [TokenDescriptor]
	sp: Integer
{Any}:
	setPool (p: as pool)
	do
		pool := p
	end // pool
	styleC: Boolean
	// C P
	// < [ less_token
	// > ] greater_token
	// { do
	// } end
	// [ {
	// ] {
	setCmode
	do
		styleC := true
	end // setCmode
	setPmode
	do
		styleC := false
	end // setPmode
	systemMode: Boolean
	enableSystemMode
	do
		systemMode := true
	end // enableSystemMode
	disableSystemMode
	do
		systemMode := false
	end // disableSystemMode
	init (fileName: String)
	do
		do
			file := File(File.openRead, fileName) //.make_open_read (fileName)
			sourceFileName := fileName
			size := file.count
			token := no_token_assigned
			doextToken := no_token_assigned
			row := 1
			col := 0
			pos := 1
			buffer := new String(64) // probably 64 bytes is the rigth run-time buffer size
			toRead := true
			keywords := (
				"<EOF>",
				"<identifier>", 
				"<comment>",
				"<operator>", 
				"<integer_const>",
				"<real_const>",
				"<string_const>",
				"<char_const>",
				"<illegal>",
				"alias",
				"as",
				"concurrent",
				"const",
				"do",
				"else",
				"elsif",
				"end",
				"ensure",
				"extend",
				"final",
				"foreign",
				"if",
				"in",
				"init",
				"is",
				"new",
				"old",
				"override",
				"pure",
				"raise",
				"ref",
				"require",
				"return",
				"rigid",
				"rtn",
				"safe",
				"select",
				"system",
				"this",
				"type",
				"use",
				"val",
				"var",
				"virtual",
				"when",
				"while",
				"(", 
				")", 
				"{", 
				"}", 
				"[", 
				"]", 
				"~", 
				":=", 
				":", 
				",", 
				"..",
				".",
				"?",
				";",
				"|",
				"=>",
				"end // if",
				"end // block",
				"end // type",
				"end // routine",
				"end // loop",
				"<",
				">",
				"-",
				"->"
			)
		when FailedToOpenFile do
			?file
			size := -1
			token := no_token_assigned
		end //do
	end // init

	isReady: Boolean
	do
		return file /= Void
	end // isReady
	
	close
	do
		if file is File do
			file.close
			?file
			buffer:= new String (64)
			//toRead := false
			size := 0
		end // if
	end // close

	/* // Bug in parser !!!
	eof_token,
	identifier_token,
	comment_token,	
	operator_token,
	integer_const_token,
	real_const_token,
	string_const_token,
	char_const_token,
	illegal_token,	
	alias_token,	
	as_token,		
	concurrent_token,
	const_token,	
	do_token,		
	else_token,
	elsif_token,
	end_token,		
	enum_token,
	ensure_token,
	extend_token,
	final_token,
	foreign_token,
	if_token,	
	in_token,
	init_token,
	is_token,		
	new_token,
	old_token,
	override_token,
	pure_token,	
	raise_token,
	ref_token,
	require_token,
	return_token,
	rigid_token,
	rtn_token,
	safe_token,
	select_token,
	system_token,
	this_token,
	unit_token,
	use_token,
	val_token,
	var_token,
	virtual_token,
	when_token,
	while_token,
	left_paranthesis_token,
	right_paranthesis_token,
	left_curly_bracket_token,
	right_curly_bracket_token,
	left_square_bracket_token,
	right_square_bracket_token,
	tilda_token,
	assignment_token,
	colon_token,
	comma_token,
	period_token,
	dot_token,
	detach_token,
	semicolon_token,
	bar_token,
	one_line_function_token,
	end_if_expected,
	end_block_expected,
	end_unit_expected,
	end_routine_expected,
	end_loop_expected,
	less_token,
	greater_token,
	minus_token,
	implies_token
	: Integer is unique */
	
	tabSize: Integer is 4

	nextWithSemicolon (checkSemicolonAfter: Boolean)
	do
		if checkSemicolonAfter do
			returnSemicolon := true
		end // if
		nextToken	
		if checkSemicolonAfter do
			returnSemicolon := false
		end // if
	end // nextWithSemicolon
	
	nextToken
	require	
		file is File /// scanner_is_ready: 
	do
		toLeave is false
		foundE is false
		foundEsign is false
		numberOfComments is 0
		tknDsc is new TokenDescriptor 

		if sp > 0 do
			tknDsc := stack (sp) 
			token := tknDsc.token
			tokenRow := tknDsc.tokenRow
			tokenCol := tknDsc.tokenCol
			buffer := tknDsc.buffer
			sp := sp + 1
			if sp > stack.count do
				flush
			end
		elsif doextToken /= no_token_assigned do
			setToken (doextToken)
			tokenRow := doextTokenRow
			tokenCol := doextTokenCol
			doextToken := no_token_assigned
		elsif pos > size do
			if ch = '' do //'%U'
				tokenRow := row
				tokenCol := col
				setTokenNoRead (eof_token)
			else
				if ch 
				case  ';'
					if returnSemicolon do
						setToken (semicolon_token)
						returnSemicolon := false
					end // if
				case  '('
					setToken (left_paranthesis_token)
				case  ')'
					setToken (right_paranthesis_token)
				case  '?'
					setToken (detach_token)				
				case  '|'
					setToken (bar_token)
				case  ','
					setToken (comma_token)
				case  '{'
					//if styleC do
					//	// C P
					//	// { do
					//	setToken (do_token)
					//else
						setToken (left_curly_bracket_token)
					//end // if
				case  '}'
					//if styleC do
					//	// C P
					//	// } end
					//	setToken (end_token)
					//else
						setToken (right_curly_bracket_token)
					//end // if
				case  '['
					//if styleC do
					//	// C P
					//	// [ {
					//	setToken (left_curly_bracket_token)
					//else
						setToken (left_square_bracket_token)
					//end // if
				case  ']'
					//if styleC do
					//	// C P
					//	// ] }
					//	setToken (right_curly_bracket_token)					
					//else
						setToken (right_square_bracket_token)
					//end // if
				case  '~'
					setToken (tilda_token)
				case  '-'
					setToken (minus_token)
				case  'A'..'Z', 'a'..'z' 
					if toRead do
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					else
						tokenRow := row
						tokenCol := col
						setCharBuff (ch)
						token := identifier_token
					end // if
				case  '0' .. '9'
					if toRead do
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					else
						tokenRow := row
						tokenCol := col
						setCharBuff (ch)
						token := integer_const_token
					end // if
				else
					if returnSemicolon do // newline is treated as separator as well where necessary
						setToken (semicolon_token)
						ch := ';'
						returnSemicolon := false
					else
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					end // if
				end // if
				ch := '' // '%U'
			end // if
		else
			token := no_token_assigned
			if toRead do
				if buffer.count > 0 do
					clear_buffer
				end
			else
				// Bug in parser
				if ch 
				case  '\t', ' ', ';'
					if buffer.count > 0 do
						clear_buffer
					end
				else
					setCharBuff (ch)
				end // if
			end
			while pos <= size and then token = no_token_assigned do
				if toRead do
					getNextChar
				else	
					toRead := true
				end
				tokenCol := col
				tokenRow := row
				if ch 
				case  ' ' // just skip
				case  '\r' // step back the col counter
					col := col - 1
				case  '\t' // just skip
					col := col + tabSize - 1
				case  '\n' 
					row := row + 1
					col := 0
					if returnSemicolon do // newline is treated as separator as well where necessary
						setToken (semicolon_token)
						ch := ';'
						returnSemicolon := false
					end // if
				case  ';' 
					if returnSemicolon do
						setToken (semicolon_token)
						returnSemicolon := false
					end // if
				case  ':' 
					if pos <= size do
						getNextChar
						if ch = '=' do
							setToken (assignment_token)
						else	
							setTokenNoRead (colon_token)
						end
					else
						setTokenNoRead (colon_token)
					end
				case  '/'  // may be a comment as well !!!
					if pos > size do
						token := operator_token
						setCharBuff ('/')
						toRead := false
					else
						getNextChar
						if ch 
						case  '*'  // Multi-line comment start
							if buffer.count > 0 do
								clear_buffer
							end
							// scan till */ found if not illegal token
							toLeave := false
							numberOfComments := 1
							while pos <= size and then not toLeave do
								getNextChar
								if ch 
								case  '/'
									if pos <= size do
										getNextChar
										if ch = '*' do
											numberOfComments := numberOfComments + 1
											buffer.append_character ('/')
											buffer.append_character (ch)
										end // if
									else
										buffer.append_character (ch)
									end // if
								case  '*' 
									if pos <= size do
										getNextChar
										if ch = '/' do
											numberOfComments := numberOfComments - 1
											if numberOfComments = 0 do
												// end of comment
												if returnComments do
													token := comment_token
												else
													token := no_token_assigned							
												end // if
												toLeave := true
											else
												buffer.append_character ('*')
												buffer.append_character (ch)
											end // if
										else	
											buffer.append_character ('*')
											buffer.append_character (ch)
										end // if
									else
										token := illegal_token
										toRead := false
										toLeave := true
									end	// if					
								case  '\n' 
									buffer.append_character ('\n')
									row := row + 1
									col := 0
								case  '\r' 
									buffer.append_character ('\r')
								else
									buffer.append_character (ch)
								end // if
							end // loop
							if pos > size and then token /= comment_token do
								token := illegal_token
								toRead := false
							end
						case  '/'  // One line comment
							token := comment_token
							if buffer.count > 0 do
								clear_buffer
							end
							toLeave := false
							while pos <= size and then not toLeave do
								getNextChar
								if ch 
								case  '\r'  // just skip
								case  '\n'  // end of comemnt line
									row := row + 1
									col := 0
									toLeave := true
								else
									buffer.append_character (ch)
								end
							end // loop
							if not returnComments do
								if returnSemicolon do // newline is treated as separator as well where necessary
									setToken (semicolon_token)
									ch := ';'
									returnSemicolon := false
								else
									token := no_token_assigned							
								end // if
							end // if
						case  '=', '<', '>', '+', '-', '\', '^', '&', '|', '~'
							token := operator_token
							setCharBuff ('/')
							buffer.append_character (ch)
						else	
							token := operator_token
							setCharBuff ('/')
							toRead := false
						end // if
					end
				case  '=' , '<', '>', '+', '-', '*', '\', '^', '&' 
					token := operator_token
					setCharBuff (ch)
					if pos <= size do
						getNextChar
						if ch 
						case  '=' , '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|'
							buffer.append_character (ch)
							if buffer.item (1) = '=' and then buffer.item (2) = '>' do
								token := one_line_function_token
							elsif buffer.item (1) = '-' and then buffer.item (2) = '>' do
								token := implies_token
							end // if
						else	
							//if styleC do
								if buffer.item (1)
								case  '<'
									// C P
									// < [
									setToken (less_token)
								case  '>'
									// C P
									// > ]
									setToken (greater_token)
								case  '-'
									setToken (minus_token)
								else
								end // if
							//end // if
							toRead := false
						end
					else
						toRead := false
					end
				case  '('
					setToken (left_paranthesis_token)
				case  ')' 
					setToken (right_paranthesis_token)
				case  '?' 
					setToken (detach_token)				
				case  '|' 
					setCharBuff (ch)
					if pos <= size do
						getNextChar
						if ch 
						case  '=', '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|' 
							buffer.append_character (ch)
							token := operator_token
						else	
							setToken (bar_token)
							toRead := false
						end // if
					else
						setToken (bar_token)
						toRead := false
					end // if
				case  '.'
					if pos <= size do
						getNextChar
						if ch
						case  '.'
							setToken (period_token)
						else	
							setTokenNoRead (dot_token)
						end // if
					else
						setTokenNoRead (dot_token)
					end
				case  '"' // start of String constant
					toLeave := false
					while pos <= size and then not toLeave do
						getNextChar
						if ch 
						case  '"' 
							token := string_const_token
							toLeave := true
						case  '\'  // process control Characters in Slang source mode 
							if systemMode do
								buffer.append_character ('\')
							elsif pos <= size do
								getNextChar
								if ch 
								case  'n', 'N'
									buffer.append_character ('\n')
								case  'r', 'R' 
									buffer.append_character ('\r')
								case  't', 'T' 
									buffer.append_character ('\t')
								case  '\' 
									buffer.append_character ('\')
								else	
									token := illegal_token
									buffer.append_character ('\')
									buffer.append_character (ch)
									toRead := false
									toLeave := true
								end
							else
								token := illegal_token
								buffer.append_character ('\')
								toRead := false
							end
						else
							buffer.append_character (ch)
						end
					end // loop
					if pos > size and then token /= string_const_token do
						token := illegal_token
						toRead := false
					end
				case  '%'  // start of char constant
					if pos <= size do
						token := char_const_token
						getNextChar
						if ch
						case  '%' 
							buffer := ""
						case  '\'  // process control Characters!!!
							if pos <= size do
								getNextChar
								if ch 
								case  'n', 'N' 
									buffer := "\n"
									getNextChar
									if ch /= '\' do
										buffer.append_character (ch)
										token := illegal_token
										toRead := false
									end // if
								case  'r', 'R'
									buffer := "\r"
									getNextChar
									if ch /= '\' do
										buffer.append_character (ch)
										token := illegal_token
										toRead := false
									end // if
								case  't', 'T'
									buffer := "\t"
									getNextChar
									if ch /= '\' do
										buffer.append_character (ch)
										token := illegal_token
										toRead := false
									end // if
								case  '\'
									buffer := "\\"
									getNextChar
									if ch /= '\' do
										buffer.append_character (ch)
										token := illegal_token
										toRead := false
									end // if
								case  '%' 
									buffer := "\\"
								else	
									token := illegal_token
									buffer := "'"
									buffer.append_character (ch)
									toRead := false
								end
							else
								token := illegal_token
								buffer := "\\"
								buffer.append_character (ch)
								toRead := false
							end
						else	
							buffer := ""
							buffer.append_character (ch)
							if pos <= size do
								getNextChar
								if ch /= '\' do
									token := illegal_token
									buffer := "'"
									buffer.append_character (ch)
									toRead := false
								end // if
							else
								token := illegal_token
								buffer := "'"
								buffer.append_character (ch)
								toRead := false
							end // if
						end // if
					else
						token := illegal_token
						buffer := "\\"
						toRead := false
					end	// if
				case  ','
					setToken (comma_token)
				case  '{'
					//if styleC do
					//	// C P
					//	// { do
					//	setToken (do_token)
					//else
						setToken (left_curly_bracket_token)
					//end // if
				case  '}' 
					//if styleC do
					//	// C P
					//	// } end
					//	setToken (end_token)
					//else
						setToken (right_curly_bracket_token)
					//end // if
				case  '[' 
					//if styleC do
					//	// C P
					//	// [ {
					//	setToken (left_curly_bracket_token)
					//else
						setToken (left_square_bracket_token)
					//end // if
				case  ']' 
					//if styleC do
					//	// C P
					//	// ] }
					//	setToken (right_curly_bracket_token)					
					//else
						setToken (right_square_bracket_token)
					//end // if
				case  '~' 
					setCharBuff (ch)
					if pos <= size do
						getNextChar
						if ch 
						case  '=', '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|' 
							buffer.append_character (ch)
							token := operator_token
						else	
							setToken (tilda_token)
							toRead := false
						end
					else
						setToken (tilda_token)
						toRead := false
					end // if
				case  'A' .. 'Z', 'g', 'h', 'j', 'k', 'l', 'm', 'q', 'x', 'y', 'z' // start of identifier
					token := identifier_token
					setCharBuff (ch)
					toLeave := false
					while pos <= size and then not toLeave do
						getNextChar
						if ch
						case  'A'..'Z', 'a'..'z', '0'..'9', '_'
							buffer.append_character (ch)
						else
							toRead := false						 
							toLeave := true
						end
					end
				case  'a' .. 'f', 'i', 'n', 'o', 'p', 'r'..'w' // start of keyword or identifier
					// or hex constant in form of ffffh which is not supported!
					setCharBuff (ch)
					toLeave := false
					while pos <= size and then not toLeave do
						getNextChar
						if ch 
						case  'A'..'Z', 'a'..'z', '0'..'9', '_'
							buffer.append_character (ch)
						else
							toRead := false
							toLeave := true
						end
					end // while
					token := getKeywordPos
				case  '0'  // start of number constant
					// 0x/X hex_number  example: 0xFFF51 
					// 0b/B bit_number  example: 0b00001 
					// 0o/O octa_number example: 0o77777
					// number.number E +/- number
					// number ..
					// number b/B/h/H/o/O
					// Optional notation
					// 00001b // binary
					// 0FFF51h // hex, let it start from 0 please .... otherwise it will be identifier :-)
					// 77777o // octa
					if pos <= size do
						getNextChar
						if ch 
						case  'x', 'X' // hex constant
							toLeave := false
							// setCharBuff ('0')
							// buffer.append_character (ch)
							// buffer will have only heximal digits !!!
							buffer := ""
							while pos <= size and then not toLeave do
								getNextChar
								if ch 
								case  '0'..'9', 'A'..'F', 'a'..'f'
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertHexToDecimal
									toRead := false
									toLeave := true
								end // if
							end // while
							if pos > size do
								token := integer_const_token
								convertHexToDecimal
								toRead := false
							elsif buffer.count = 0 do
								token := illegal_token
							end
						case  'b', 'B' // bit constant
							toLeave := false
							// setCharBuff ('0')
							// buffer.append_character (ch)
							// buffer will have 0 and 1 only -> later convertable into integer
							buffer := ""
							while pos <= size and then not toLeave do
								getNextChar
								if ch 
								case  '0', '1'
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertBitToDecimal
									toRead := false
									toLeave := true
								end // if
							end // while
							if pos > size do
								token := integer_const_token
								convertBitToDecimal
								toRead := false
							elsif buffer.count = 0 do
								token := illegal_token
							end
						case  'o', 'O'  // octa constant
							toLeave := false
							// setCharBuff ('0')
							// buffer.append_character (ch)
							// buffer will have only octamal digits !!!
							buffer := ""
							while pos <= size and then not toLeave do
								getNextChar
								if ch 
								case  '0'..'7'
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertOctaToDecimal
									toRead := false
									toLeave := true
								end // if
							end // while
							if pos > size do
								token := integer_const_token
								convertOctaToDecimal
								toRead := false
							elsif buffer.count = 0 do
								token := illegal_token
							end
						else
							// 0number.number E +/- number
							// 0number ..
							// 0number b/B/h/H/o/O
							// 0.number
							// 0..
							// 0any
							setCharBuff ('0')
							if	ch
							case  '0'..'9'
								scan_number_constant 
							case  '.'
								if pos <= size do
									getNextChar
									if ch 
									case  '.'
										token := integer_const_token
										integer_value := buffer.to_integer										
										setdoextToken (period_token)
										toRead := true
									case  '0'..'9' // Real constant 0.digits
										token := real_const_token
										foundE := false
										foundEsign := false
										buffer.append_character ('.')
										toLeave := false
										buffer.append_character (ch)
										while pos <= size and then not toLeave do
											getNextChar
											if ch
											case  '0'..'9'
												buffer.append_character (ch)
											case  'e', 'E' // not tested !!!
												if foundE do
													toRead := false
													toLeave := true
												else
													buffer.append_character ('E')
													foundE := true
												end // if
											case  '+', '-'
												if foundE do
													// one more bool flag is required ... to prevent 0.3E++//
													if foundEsign do
														toRead := false
														toLeave := true
													else
														foundEsign := true
														buffer.append_character (ch)
													end // if
												else
													toRead := false
													toLeave := true
												end // if
											else
												toRead := false
												toLeave := true
											end // if
										end // loop
									else
										token := integer_const_token
										integer_value := buffer.to_integer
										setdoextToken (dot_token)
										toRead := false
									end
								else
									token := integer_const_token
									integer_value := buffer.to_integer									
									setdoextToken (dot_token)
									toRead := false
								end
							else
								token := integer_const_token
								integer_value := buffer.to_integer
								toRead := false
							end
							toLeave := true
						end
					else
						setCharBuff (ch)
						token := integer_const_token
						integer_value := buffer.to_integer
						toRead := false
					end
				case  '1' .. '9' // start of number constant
					scan_number_constant
				else
					buffer.append_character (ch)
					token := illegal_token
				end // if
			end // loop
			if pos > size and then token = no_token_assigned do
				tokenRow := row
				tokenCol := col
				setTokenNoRead(eof_token)
			end // if
		end
//print("\ttoken: " + tokenName(token) + "\n")
	end

	integer_value: Integer

	convertHexToDecimal
	do
		integer_value := 0
		factor is 1
		i is buffer.count
		while i > 0 do
			if buffer(i)
			case  '1'
				integer_value := integer_value +  1 * factor
			case  '2'
				integer_value := integer_value +  2 * factor
			case  '3'
				integer_value := integer_value +  3 * factor
			case  '4'
				integer_value := integer_value +  4 * factor
			case  '5'
				integer_value := integer_value +  5 * factor
			case  '6'
				integer_value := integer_value +  6 * factor
			case  '7'
				integer_value := integer_value +  7 * factor
			case  '8'
				integer_value := integer_value +  8 * factor
			case  '9'
				integer_value := integer_value +  9 * factor
			case  'A', 'a'
				integer_value := integer_value +  10 * factor
			case  'B', 'b'
				integer_value := integer_value +  11 * factor
			case  'C', 'c'
				integer_value := integer_value +  12 * factor
			case  'D', 'd'
				integer_value := integer_value +  13 * factor
			case  'E', 'e'
				integer_value := integer_value +  14 * factor
			case  'F', 'f'
				integer_value := integer_value +  15 * factor
			end // if
			i := i - 1
			factor := factor * 16
		end // loop
	end // if
	convertOctaToDecimal
	do
		integer_value := 0
		factor is 1
		i is buffer.count
		while i > 0 do
			if buffer (i)
			case  '1'
				integer_value := integer_value +  1 * factor
			case  '2'
				integer_value := integer_value +  2 * factor
			case  '3'
				integer_value := integer_value +  3 * factor
			case  '4'
				integer_value := integer_value +  4 * factor
			case  '5'
				integer_value := integer_value +  5 * factor
			case  '6'
				integer_value := integer_value +  6 * factor
			case  '7'
				integer_value := integer_value +  7 * factor
			end // if
			i := i - 1
			factor := factor * 8
		end // loop
	end // if
	convertBitToDecimal
	do
		integer_value := 0
		factor is 1
		i is buffer.count
		while i > 0 do
			if buffer (i) = '1' do
				integer_value := integer_value +  1 * factor
			end // if
			i := i - 1
			factor := factor * 2
		end // loop
	end // if


	enableComments
	do
		returnComments := true
	end // enableComments

	disableComments
	do
		returnComments := false
	end // disableComments
	
{}:

	returnComments: Boolean
	
	scan_number_constant
	require
		'0' <= ch and then ch <= '9' /// current_char_is_digit
	do
		// number.number E +/- number
		// number ..
		// number b/B/h/H/o/O
		isRealConst is false
		isBitConst is ch = '1'
		isOctaConst is ch in '1'..'7'
		setCharBuff(ch)
		token := integer_const_token
		foundEsign is false
		foundE is false
		toLeave is false
		while pos <= size and then not toLeave do
			getNextChar
			if ch
			case  '0', '1'
				buffer.append_character (ch)
			case  '2'..'7'
				isBitConst := false
				buffer.append_character (ch)
			case  '8'..'9'
				isBitConst := false
				isOctaConst := false
				buffer.append_character (ch)
			////////////////////
			case  'e', 'E' // not tested !!!
				if foundE do
					toRead := false
					toLeave := true
				else
					buffer.append_character ('E')
					foundE := true
				end // if
			case  '+', '-'
				if foundE do
					// one more bool flag is required ... to prevent 0.3E++//
					if foundEsign do
						toRead := false
						toLeave := true
					else
						foundEsign := true
						buffer.append_character (ch)
					end // if
				else
					toRead := false
					toLeave := true
				end // if
			//////////////////////
			case  '.' // Real or period or syntax error 
				if pos <= size do
					getNextChar
					if ch
					case  '.'
						setdoextToken (period_token)
						toLeave := true
						toRead := true
					case  '0'..'9' // fractional part
						isRealConst := true
						isBitConst := false
						isOctaConst := false
						buffer.append_character (ch)
					else	
						setdoextToken (dot_token)
						toRead := false
						toLeave := true
					end
				else
					setdoextToken (dot_token)
					toLeave := true
				end // if
			
// Not supported so far
//						: 'e', 'E' :

// Not supported so far
//						: 'b', 'B' :
//						: 'h', 'H' :
//						: 'o', 'O' :
			else
				toRead := false
				if isRealConst do
					token := real_const_token
				end
				toLeave := true
			end // if
		end // loop
		if token = integer_const_token do
			if isBitConst do
				convertBitToDecimal
			elsif isOctaConst do
				convertOctaToDecimal
			else
				integer_value := buffer.to_integer
			end // if
		end // if
	end // scan_number_constant

	doextToken: Integer
	doextTokenRow: Integer
	doextTokenCol: Integer
	
	setdoextToken(t: Integer)
	do
		doextToken := t
		doextTokenRow := tokenRow
		doextTokenCol := tokenCol
	end // setdoextToken
	
	keywords: Array [String]
	file: ?File
	row, col, pos, size: Integer
	toRead: Boolean
	buffer: String
	ch: Character
	returnSemicolon: Boolean
	
	getKeywordPos: Integer
	do
		first_ch is buffer (1)
		buff_len is buffer.count
		if first_ch 
		case  'a'  // "alias", "as"
			if buff_len
			case  2 
				if buffer (2) = 's' do
					return as_token
				else
					return identifier_token
				end
			case  5 
				if buffer.is_equal (keywords (alias_token)) do
					return alias_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'c'  // "concurrent", "const"
			if buff_len
			case  5 
				if buffer.is_equal (keywords (const_token)) do
					return const_token
				else
					return identifier_token
				end
			case  10 
				if buffer.is_equal (keywords (concurrent_token)) do
					return concurrent_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'd'  // "do"
			if buff_len = 2 and then buffer (2) = 'o' do
				if styleC do
					return illegal_token // identifier_token
				else
					return do_token
				end // if
			else
				return identifier_token
			end
		case  'e'  // "else" 4, "elsif" -5, "end" - 3, "ensure" 6, "extend" - 6
			if buff_len 
			case  3 // end
				if buffer (2) = 'n' and then buffer (3) = 'd' do
					if styleC do
						return illegal_token // identifier_token
					else
						return end_token
					end // if
				else
					return identifier_token
				end
			case  4 // else
				if buffer.is_equal (keywords (else_token)) do
					return else_token
				else
					return identifier_token
				end
			case  5 // elsif
				if buffer.is_equal (keywords (elsif_token)) do
					return elsif_token
				else
					return identifier_token
				end
			case  6 // "ensure", "extend"
				if buffer (2) = 'n' and then buffer.is_equal (keywords (ensure_token)) do
					return ensure_token
				elsif buffer (2) = 'x' and then buffer.is_equal (keywords (extend_token)) do
					return extend_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'f' // "final" - 5, "foreign" - 7
			if buff_len
			case  5 
				if buffer.is_equal (keywords (final_token)) do
					return final_token
				else
					return identifier_token
				end
			case  7 
				if buffer.is_equal (keywords (foreign_token)) do
					return foreign_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'i'  // "if", "in", "init", "is"
			if buff_len
			case  2 
				if buffer (2) = 's' do
					return is_token
				elsif buffer (2) = 'f' do
					return if_token
				elsif buffer (2) = 'n' do
					return in_token
				else
					return identifier_token
				end
			case  4 
				if buffer.is_equal (keywords (init_token)) do
					return init_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'n'  // "new"
			if buff_len = 3 and then buffer (2) = 'e' and then buffer (3) = 'w' do
				return new_token
			else
				return identifier_token
			end
		case  'o'  // "old", "override"
			if buff_len
			case  3 
				if buffer (2) = 'l' and then buffer (3) = 'd' do
					return old_token
				else
					return identifier_token
				end
			case  8 
				if buffer.is_equal (keywords (override_token)) do
					return override_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'p' // "pure"
			if buff_len = 4 and then buffer (2) = 'u' and then buffer (3) = 'r' and then buffer (4) = 'e' do
				return pure_token
			else
				return identifier_token
			end
		case  'r' // "raise" - 5, "ref" - 3, "rename" - 6, "require" - 7, "return" - 6, "rigid" - 5, "rtn" - 3
			if buff_len
			case  3 // ref rtn
				if buffer (2) = 'e' and then buffer (3) = 'f' do
					return ref_token
				elsif buffer (2) = 't' and then buffer (3) = 'n' do
					return rtn_token
				else
					return identifier_token
				end
			case  5 // raise rigid
				if buffer.is_equal (keywords (rigid_token)) do
					return rigid_token
				elsif buffer.is_equal (keywords (raise_token)) do
					return raise_token
				else
					return identifier_token
				end
			case  6 // return
				if buffer.is_equal (keywords (return_token)) do
					return return_token
				else
					return identifier_token
				end
			case  7 // "require"
				if buffer.is_equal (keywords (require_token)) do
					return require_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  's' // "safe", "select", "system"
			if buff_len 
			case  4 
				if buffer (2) = 'a' and then buffer (3) = 'f' and then buffer (4) = 'e' do
					return safe_token
				else
					return identifier_token
				end
			case  6 
				if buffer.is_equal (keywords (select_token)) do
					return select_token
				elsif systemMode and then buffer.is_equal (keywords (system_token)) do
					return system_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end			
		case  't' // "this"
			if buff_len = 4 and then buffer (2) = 'h' and then buffer (3) = 'i' and then buffer (4) = 's' do
				return this_token
			else
				return identifier_token
			end
		case  'u' // "type", "use"
			if buff_len
			case  3 
				if buffer (2) = 's' and then buffer (3) = 'e' do
					return use_token
				else
					return identifier_token
				end
			case  4 
				if buffer (2) = 'n' and then buffer (3) = 'i'  and then buffer (4) = 't' do
					return unit_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'v'  // "val", "var", "virtual"
			if buff_len
			case  3 
				if buffer (2) = 'a' and then buffer (3) = 'r' do
					return var_token
				elsif buffer (2) = 'a' and then buffer (3) = 'l' do
					return val_token
				else
					return identifier_token
				end
			case  7 
				if buffer.is_equal (keywords (virtual_token)) do
					return virtual_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end
		case  'w'  // "when", "while"
			if buff_len
			case  4
				if buffer (2) = 'h' and then buffer (3) = 'e' and then buffer (4) = 'n' do
					return when_token
				else
					return identifier_token
				end
			case  5 
				if buffer.is_equal (keywords (while_token)) do
					return while_token
				else
					return identifier_token
				end
			else
				return identifier_token
			end			
		else
			return identifier_token
		end
	ensure
		return > 0 /// token_assigned
	end
	
	getNextChar 
	require	
		pos <= size /// not_eof:
		pos > 0 /// position_within_file: 
		toRead /// to_read_is_set: 
	do
		file.read_character
		pos := pos + 1
		col := col + 1
		ch := file.last_character
	end // getNextChar

	clear_buffer
	require
		buffer.count > 0 /// non_empty_buffer: 
	do
		buffer := ""
	ensure
		buffer.count = 0 /// buffer_is_empty: 
	end // clear_buffer

	setCharBuff (aChar: Character)
	do
		buffer := " "
		buffer.put (aChar, 1)
	end // setCharBuff

	setToken (aToken: Integer)
	//require
	//	keyword_token: alias_token <= aToken and then aToken <= one_line_function_token or else aToken = eof_token
	do	
		token := aToken
		buffer := keywords (token)
	end // setToken

	setTokenNoRead (aToken: Integer)
	//require
	//	keyword_token: alias_token <= aToken and then aToken <= one_line_function_token or else aToken = eof_token
	do
		token  := aToken
		buffer := keywords (token)
		toRead := false
	end // setTokenNoRead
	
	no_token_assigned: Integer is -1
	
end // type SLang_Scanner
