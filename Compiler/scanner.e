class LinePosition
feature {Any}
	row: Integer
invariant
	valid_row: row >= 0
end -- class LinePosition
class SourcePosition
inherit
	LinePosition
	end
feature {Any}
	col: Integer
	set_rc (r, c: Integer) is
	require
		valid_row: r > 0
		valid_column: c > 0
	do
		row := r
		col := c
	end -- set_rc
	setSourcePosition (srcp: expanded SourcePosition) is
	do
		row := srcp.row
		col := srcp.col
	end -- setSourcePosition
	toSourcePosition: expanded SourcePosition is
	do
		Result.set_rc (row, col)
	end -- toSourcePosition
invariant
	valid_column: col >= 0
end -- class SourcePosition

class TokenDescriptor
create
	init
feature {Any}
	token: Integer -- current token
	tokenRow: Integer -- current token row
	tokenCol: Integer -- current token column
	buffer: String
	init (t, r, c: Integer; b: String) is
	do
		token:= t
		tokenRow:= r
		tokenCol:= c
		buffer:= b
	end -- init
end -- class TokenDescriptor

class SLang_Scanner
inherit
	Server
creation
	init
feature {Any}
	token: Integer -- current token
	tokenRow: Integer -- current token row
	tokenCol: Integer -- current token column

	source_position: expanded SourcePosition is
	do
		Result.set_rc (tokenRow, tokenCol)
	end -- source_position

	tokenString: String is -- current token string representation
--	require
--		string_pool_set: pool /= Void
	do
--		Result := pool.add_it (buffer)
		Result := buffer
	end -- tokenString

	tokenValue: Any is -- current token value, Void if not manifest constant
	do
		inspect
			token
		when integer_const_token then
			Result := integer_value
		when real_const_token    then
			Result := buffer.to_real
		when string_const_token  then
			Result := buffer 
		when char_const_token    then
			Result := buffer.item (1)
		else
		end -- inspect		
	ensure
		Result /= Void implies (
			token = integer_const_token or else 
			token = real_const_token or else
			token = string_const_token or else
			token = char_const_token
		)
	end -- tokenValue
	
	sourceFileName: String
	
	tokenName (aToken: Integer): String is
	require
		valid_token: 1<= aToken and then aToken <= keywords.count
	do
		Result := keywords.item (aToken)
	ensure
		tokenName_not_void: Result /= Void
	end -- tokenName

	push is
	local
		tknDsc: TokenDescriptor
	do
		create tknDsc.init (token, tokenRow, tokenCol, buffer)
		if stack = Void then
			stack := <<tknDsc>>
		else
			stack.force (tknDsc, stack.count + 1)
		end -- if
	end -- if
	
	revert is
	do
		sp := 1
		nextToken		
	end -- revert
	
	flush is
	do
		stack.resize (1, 0)
		sp := 0
	end -- if

	blockStart: Boolean is
	do
		Result := Cmode and then token = left_curly_bracket_token or else token = do_token 
	end -- blockStart
	
	blockEnd: Boolean is
	do
		Result := Cmode and then token = right_curly_bracket_token	or else token = end_token 
	end -- blockEnd

	visibilityStart: Boolean is
	do
		Result := Cmode and then token = left_square_bracket_token or else token = left_curly_bracket_token
	end -- visibilityStart

	visibilityEnd: Boolean is
	do
		Result := Cmode and then token = right_square_bracket_token or else token = right_curly_bracket_token
	end -- visibilityEnd
	
	genericsStart: Boolean is
	do
		Result := Cmode and then token = less_token or else token = left_square_bracket_token
	end -- genericsStart

	genericsEnd: Boolean is
	do
		Result := Cmode and then token = greater_token or else token = right_square_bracket_token
	end -- genericsEnd


feature {None}
	pool: Sorted_Array [String]
	stack: Array [TokenDescriptor]
	sp: Integer
feature {Any}

	setPool (p: like pool) is
	do
		pool := p
	end -- pool
	Cmode: Boolean
	-- C P
	-- < [ less_token
	-- > ] greater_token
	-- { do
	-- } end
	-- [ {
	-- ] {
	setCmode is
	do
		Cmode := True
	end -- setCmode
	setPmode is
	do
		Cmode := False
	end -- setPmode
	systemMode: Boolean
	enableSystemMode is
	do
		systemMode := True
	end -- enableSystemMode
	disableSystemMode is
	do
		systemMode := False
	end -- disableSystemMode
	init (fileName: String) is
	require	
		non_void_file_name: fileName /= Void
	local	
		wasError: Boolean
	do
		if wasError then
			file := Void
			size := -1
		else
			token := no_token_assigned
			theNextToken := no_token_assigned
			row := 1
			col := 0
			pos := 1
			create file.make_open_read (fileName)
			timeStamp := fs.file_data(fileName).time.rounded
			sourceFileName := fileName
			size := file.count
			create buffer.make (64) -- probably 64 bytes is the rigth run-time buffer size
			toRead := True
			keywords := <<
				"<EOF>",
				"<identifier>", 
				"<type name>",
				"<comment>",
				"<operator>", 
				"<integer_const>",
				"<real_const>",
				"<string_const>",
				"<char_const>",
				"<illegal>",
				"alias",
				"as",
				"build",
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
				"is",
				"new",
				"none",
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
				"this",
				"unit",
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
				"end // unit",
				"end // routine",
				"end // loop",
				"<",
				">",
				"->"
			>>
		end
	rescue
		wasError := True
		retry
	end -- init

	isReady: Boolean is
	do
		Result := file /= Void
	end -- isReady
	
	close is
	do
		if file /= Void then
			file.close
			file := Void
			buffer := Void
			--toRead := False
			size := 0
		end -- if
	end -- close

	eof_token,
	identifier_token,
	type_name_token,
	comment_token,	
	operator_token,
	integer_const_token,
	real_const_token,
	string_const_token,
	char_const_token,
	illegal_token,	
	alias_token,	
	as_token,	
	build_token,
	concurrent_token,
	const_token,	
	do_token,		
	else_token,
	elsif_token,
	end_token,		
	ensure_token,
	extend_token,
	final_token,
	foreign_token,
	if_token,	
	in_token,
	is_token,		
	new_token,
	none_token,
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
	implies_token
	: Integer is unique
	
	TabSize: Integer is 4

	nextWithSemicolon (checkSemicolonAfter: Boolean) is
	do
		if checkSemicolonAfter then
			returnSemicolon := True
		end -- if
		nextToken	
		if checkSemicolonAfter then
			returnSemicolon := False
		end -- if
	end -- nextWithSemicolon
	
	nextToken is
	require	
		scanner_is_ready: file /= Void
	local
		toLeave: Boolean
		Efound: Boolean
		EsignFound: Boolean
		numberOfComments: Integer
		tknDsc: TokenDescriptor 
	do
		if sp > 0 then
			tknDsc := stack.item (sp) 
			token := tknDsc.token
			tokenRow := tknDsc.tokenRow
			tokenCol := tknDsc.tokenCol
			buffer := tknDsc.buffer
			sp := sp + 1
			if sp > stack.count then
				flush
			end
		elseif theNextToken /= no_token_assigned then
			setToken (theNextToken)
			tokenRow := theNextTokenRow
			tokenCol := theNextTokenCol
			theNextToken := no_token_assigned
		elseif pos > size then
			if ch = '%U' then
				tokenRow := row
				tokenCol := col
				if tokenCol = 0 then
					tokenCol := 1
				end -- if
				setTokenNoRead (eof_token)
			else
				inspect
					ch
				when ';' then
					if returnSemicolon then
						setToken (semicolon_token)
						returnSemicolon := False
					else
						setTokenNoRead (eof_token)
					end -- if
				when '(' then
					setToken (left_paranthesis_token)
				when ')' then
					setToken (right_paranthesis_token)
				when '?' then
					setToken (detach_token)				
				when '|' then
					setToken (bar_token)
				when ',' then
					setToken (comma_token)
				when '{' then
					setToken (left_curly_bracket_token)
				when '}' then
					setToken (right_curly_bracket_token)
				when '[' then
					setToken (left_square_bracket_token)
				when ']' then
					setToken (right_square_bracket_token)
				when '~' then
					setToken (tilda_token)
				when '-' then
					setToken (operator_token)
--					setToken (minus_token)
				when 'A'..'Z' then
					if toRead then
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					else
						tokenRow := row
						tokenCol := col
						setCharBuff (ch)
						token := type_name_token
						check
							pool_set: pool /= Void
						end -- check
						buffer := pool.add_it (buffer)
					end -- if
				when 'a'..'z' then
					if toRead then
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					else
						tokenRow := row
						tokenCol := col
						setCharBuff (ch)
						token := identifier_token
						check
							pool_set: pool /= Void
						end -- check
						buffer := pool.add_it (buffer)
					end -- if
				when '0' .. '9' then 
					if toRead then
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					else
						tokenRow := row
						tokenCol := col
						setCharBuff (ch)
						token := integer_const_token
					end -- if
				else
					if returnSemicolon then -- newline is treated as separator as well where necessary
						setToken (semicolon_token)
						ch := ';'
						returnSemicolon := False
					else
						tokenRow := row
						tokenCol := col
						setTokenNoRead (eof_token)
					end -- if
				end -- inspect
				ch := '%U'
			end -- if
		else
			from
				token := no_token_assigned
				if toRead then
					if buffer.count > 0 then
						clear_buffer
					end
				else
					inspect	
						ch
					when '%T', ' ', ';' then
						if buffer.count > 0 then
							clear_buffer
						end
					else
						setCharBuff (ch)
					end
				end
			until
				pos > size or else token /= no_token_assigned
			loop
				if toRead then
					getNextChar
				else	
					toRead := True
				end
				tokenCol := col
				tokenRow := row
				inspect
					ch
				when ' ' then -- just skip
				when '%R' then -- step back the col counter
					col := col - 1
				when '%T' then -- just skip
					col := col + TabSize - 1
				when '%N' then
					next_row
					if returnSemicolon then -- newline is treated as separator as well where necessary
						setToken (semicolon_token)
						ch := ';'
						returnSemicolon := False
					end -- if
				when ';' then
					if returnSemicolon then
						setToken (semicolon_token)
						returnSemicolon := False
					end -- if
				when ':' then
					if pos <= size then
						getNextChar
						inspect
							ch
						when '=' then
							setToken (assignment_token)
						else	
							setTokenNoRead (colon_token)
						end
					else
						setTokenNoRead (colon_token)
						ch := '%U'
					end
				when '/' then -- may be a comment as well !!!
					if pos > size then
						token := operator_token
						setCharBuff ('/')
						toRead := False
					else
						getNextChar
						inspect
							ch
						when '*' then -- Multi-line comment start
							if buffer.count > 0 then
								clear_buffer
							end
							-- scan till */ found if not illegal token
							from
								toLeave := False
								numberOfComments := 1
							until
								pos > size or else toLeave
							loop
								getNextChar
								inspect
									ch
								when '/' then
									if pos <= size then
										getNextChar
										if ch = '*' then
											numberOfComments := numberOfComments + 1
											buffer.append_character ('/')
											buffer.append_character (ch)
										end -- if
									else
										buffer.append_character (ch)
									end -- if
								when '*' then
									if pos <= size then
										getNextChar
										inspect
											ch
										when '/' then
											numberOfComments := numberOfComments - 1
											if numberOfComments = 0 then
												-- end of comment
												if returnComments then
													token := comment_token
												else
													token := no_token_assigned
												end -- if
												toLeave := True
											else
												buffer.append_character ('*')
												buffer.append_character (ch)
											end -- if
										else	
											buffer.append_character ('*')
											buffer.append_character (ch)
										end -- inspect
									else
										token := illegal_token
										toRead := False
										toLeave := True
									end	-- if					
								when '%N' then
									buffer.append_character ('%N')
									next_row
								when '%R' then
									buffer.append_character ('%R')
								else
									buffer.append_character (ch)
								end -- inspect
							end -- loop
							if pos > size and then not (token = comment_token or else token = no_token_assigned) then
								token := illegal_token
								toRead := False
							end
						when '/' then -- One line comment
							token := comment_token
							if buffer.count > 0 then
								clear_buffer
							end
							from
								toLeave := False
							until
								pos > size or else toLeave
							loop
								getNextChar
								inspect
									ch
								when '%R' then -- just skip
								when '%N' then -- end of comemnt line
									next_row
									toLeave := True
								else
									buffer.append_character (ch)
								end
							end -- loop
							if not returnComments then
								if returnSemicolon then -- newline is treated as separator as well where necessary
									setToken (semicolon_token)
									ch := ';'
									returnSemicolon := False
								else
									token := no_token_assigned							
								end -- if
							end -- if
						when '=', '<', '>', '+', '-', '\', '^', '&', '|', '~', '#', '%%', '@', '!', '$' then
							token := operator_token
							setCharBuff ('/')
							buffer.append_character (ch)
						else	
							token := operator_token
							setCharBuff ('/')
							toRead := False
						end
					end
				when '=', '<', '>', '+', '-', '*', '\', '^', '&', '#', '%%', '@', '!', '$' then
					token := operator_token
					setCharBuff (ch)
					if pos <= size then
						getNextChar
						inspect
							ch
						when '=', '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|', '#', '%%', '@', '!', '$' then
							buffer.append_character (ch)
							if buffer.item (1) = '=' and then buffer.item (2) = '>' then
								token := one_line_function_token
							elseif buffer.item (1) = '-' and then buffer.item (2) = '>' then
								token := implies_token
							else
				 				-- do nothing as all is done already. 2 char operator already scanned
							end -- if
						else	
							--if Cmode then
								inspect
									buffer.item (1)
								when '<' then
									-- C P
									-- < [
									setToken (less_token)
								when '>' then
									-- C P
									-- > ]
									setToken (greater_token)
								when '-' then
									setToken (operator_token)
--									setToken (minus_token)
								else
								end -- inspect
							--end -- if
							toRead := False
						end
					else
						toRead := False
					end
				when '(' then
					setToken (left_paranthesis_token)
				when ')' then
					setToken (right_paranthesis_token)
				when '?' then
					setToken (detach_token)				
				when '|' then
					if pos <= size then
						getNextChar
						inspect
							ch
						when '=', '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|', '#', '%%', '@', '!', '$' then
							buffer := "  "
							buffer.put ('|', 1)
							buffer.put (ch, 2)
							token := operator_token
						else	
							setTokenNoRead (bar_token)
						end
					else
						setTokenNoRead (bar_token)
						ch := '%U'
					end -- if
				when '.' then 
					if pos <= size then
						getNextChar
						inspect
							ch
						when '.' then
							setToken (period_token)
						else	
							setTokenNoRead (dot_token)
						end
					else
						setTokenNoRead (dot_token)
					end
				when '"' then -- start of String constant
					from
						toLeave := False
						buffer := ""
					until
						pos > size or else toLeave
					loop
						getNextChar
						inspect
							ch
						when '"' then
							token := string_const_token
							toLeave := True
						when '\'  then -- process control Characters in Slang source mode
							if systemMode then
								buffer.append_character ('\')
							elseif pos <= size then
								getNextChar
								inspect
									ch
								when 'n', 'N' then
									buffer.append_character ('%N')
								when 'r', 'R' then
									buffer.append_character ('%R')
								when 't', 'T' then	
									buffer.append_character ('%T')
								when '\' then
									buffer.append_character ('\')
								-- Add Unicode support - UTF-8 probably
								--when 'U' then
								--when 'u' then
								else	
									token := illegal_token
									buffer.append_character ('\')
									buffer.append_character (ch)
									toRead := False
									toLeave := True
								end
							else
								token := illegal_token
								buffer.append_character ('\')
								toRead := False
							end
						else
							buffer.append_character (ch)
						end
					end -- loop
					if pos > size and then token /= string_const_token then
						token := illegal_token
						toRead := False
					end
				when '%'' then -- start of char constant
					if pos <= size then
						token := char_const_token
						getNextChar
						inspect
							ch
						when '%'' then
							buffer := ""
						when '\'  then -- process control Characters!!!
							if pos <= size then
								getNextChar
								inspect
									ch
								when 'n', 'N' then
									buffer := "%N"
									getNextChar
									if ch /= '%'' then
										buffer.append_character (ch)
										token := illegal_token
										toRead := False
									end -- if
								when 'r', 'R' then
									buffer := "%R"
									getNextChar
									if ch /= '%'' then
										buffer.append_character (ch)
										token := illegal_token
										toRead := False
									end -- if
								when 't', 'T' then	
									buffer := "%T"
									getNextChar
									if ch /= '%'' then
										buffer.append_character (ch)
										token := illegal_token
										toRead := False
									end -- if
								when '\' then
									buffer := "\"
									getNextChar
									if ch /= '%'' then
										buffer.append_character (ch)
										token := illegal_token
										toRead := False
									end -- if
								when '%'' then
									buffer := "\"								
								else	
									token := illegal_token
									buffer := "'"
									buffer.append_character (ch)
									toRead := False
								end
							else
								token := illegal_token
								buffer := "'\"
								buffer.append_character (ch)
								toRead := False
							end
						-- Add Unicode support - UTF-8 probably
						--when 'U', 'u' then
							-- scan code point
						else	
							buffer := ""
							buffer.append_character (ch)
							if pos <= size then
								getNextChar
								if ch /= '%'' then
									token := illegal_token
									buffer := "'"
									buffer.append_character (ch)
									toRead := False
								end -- if
							else
								token := illegal_token
								buffer := "'"
								buffer.append_character (ch)
								toRead := False
							end -- if
						end -- inspect
					else
						token := illegal_token
						buffer := "%'"
						toRead := False
					end	-- if
				when ',' then
					setToken (comma_token)
				when '{' then
					setToken (left_curly_bracket_token)
				when '}' then
					setToken (right_curly_bracket_token)
				when '[' then
					setToken (left_square_bracket_token)
				when ']' then
					setToken (right_square_bracket_token)
				when '~' then
					if pos <= size then
						getNextChar
						inspect
							ch
						when '=', '/', '<', '>', '+', '-', '*', '\', '^', '&', '~', '|', '#', '%%', '@', '!', '$' then
							buffer := "  "
							buffer.put ('~', 1)
							buffer.put (ch, 2)
							token := operator_token
						else	
							setTokenNoRead (tilda_token)
						end
					else
						setTokenNoRead (tilda_token)
						ch := '%U'
					end -- if
				when 'g', 'h', 'j', 'k', 'l', 'm', 'q', 'x', 'y', 'z' then -- start of identifier
					token := identifier_token
					setCharBuff (ch)
					from
						toLeave := False
					until
						pos > size or else toLeave
					loop
						getNextChar
						inspect
							ch
						when 'A'..'Z', 'a'..'z', '0'..'9', '_' then
							buffer.append_character (ch)
						else
							toRead := False						 
							toLeave := True
						end
					end -- loop
					check
						pool_set: pool /= Void
					end -- check
					buffer := pool.add_it (buffer)
				when 'A' .. 'Z' then -- start of unit name/type
					-- or hex constant in form of FFFH which is not supported!
					token := type_name_token
					setCharBuff (ch)
					from
						toLeave := False
					until
						pos > size or else toLeave
					loop
						getNextChar
						inspect
							ch
						when 'A'..'Z', 'a'..'z', '0'..'9', '_' then
							buffer.append_character (ch)
						else
							toRead := False						 
							toLeave := True
						end
					end -- loop
					check
						pool_set: pool /= Void
					end -- check
					buffer := pool.add_it (buffer)
				when 'a' .. 'f', 'i', 'n', 'o', 'p', 'r'..'w' then -- start of keyword or identifier
					-- or hex constant in form of ffffh which is not supported!
					setCharBuff (ch)
					from
						toLeave := False
					until
						pos > size or else toLeave
					loop
						getNextChar
						inspect
							ch
						when 'A'..'Z', 'a'..'z', '0'..'9', '_' then
							buffer.append_character (ch)
						else
							toRead := False
							toLeave := True
						end
					end
					token := getKeywordPos
				when '0' then -- start of number constant
					-- 0x/X hex_number  example: 0xFFF51 
					-- 0b/B bit_number  example: 0b00001 
					-- 0o/O octa_number example: 0o77777
					-- number.number E +/- number
					-- number ..
					-- number b/B/h/H/o/O
					-- Optional notation
					-- 00001b // binary
					-- 0FFF51h // hex, let it start from 0 please .... otherwise it will be identifier :-)
					-- 77777o // octa
					if pos <= size then
						getNextChar
						inspect
							ch
						when 'x', 'X' then -- hex constant
							from
								toLeave := False
								-- setCharBuff ('0')
								-- buffer.append_character (ch)
								-- buffer will have only heximal digits !!!
								buffer := ""
							until
								pos > size or else toLeave
							loop
								getNextChar
								inspect
									ch
								when '0'..'9', 'A'..'F', 'a'..'f' then
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertHexToDecimal
									toRead := False
									toLeave := True
								end
							end
							if pos > size then
								token := integer_const_token
								convertHexToDecimal
								toRead := False
							elseif buffer.count = 0 then
								token := illegal_token
							end
						when 'b', 'B' then -- bit constant
							from
								toLeave := False
								-- setCharBuff ('0')
								-- buffer.append_character (ch)
								-- buffer will have 0 and 1 only -> later convertable into integer
								buffer := ""
							until
								pos > size or else toLeave
							loop
								getNextChar
								inspect
									ch
								when '0', '1' then
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertBitToDecimal
									toRead := False
									toLeave := True
								end
							end
							if pos > size then
								token := integer_const_token
								convertBitToDecimal
								toRead := False
							elseif buffer.count = 0 then
								token := illegal_token
							end
						when 'o', 'O' then -- octa constant
							from
								toLeave := False
								-- setCharBuff ('0')
								-- buffer.append_character (ch)
								-- buffer will have only octamal digits !!!
								buffer := ""
							until
								pos > size or else toLeave
							loop
								getNextChar
								inspect
									ch
								when '0'..'7' then
									buffer.append_character (ch)
								else
									token := integer_const_token
									convertOctaToDecimal
									toRead := False
									toLeave := True
								end
							end
							if pos > size then
								token := integer_const_token
								convertOctaToDecimal
								toRead := False
							elseif buffer.count = 0 then
								token := illegal_token
							end
						else
							-- 0number.number E +/- number
							-- 0number ..
							-- 0number b/B/h/H/o/O
							-- 0.number
							-- 0..
							-- 0any
							setCharBuff ('0')
							inspect	
								ch
							when '0'..'9' then
								scan_number_constant 
							when '.' then
								if pos <= size then
									getNextChar
									inspect
										ch
									when '.' then
										token := integer_const_token
										integer_value := buffer.to_integer										
										setTheNextToken (period_token)
										toRead := True
									when '0'..'9' then -- Real constant 0.digits
										token := real_const_token
										Efound := False
										EsignFound := False
										buffer.append_character ('.')
										from
											toLeave := False
											buffer.append_character (ch)
										until
											pos > size or else toLeave
										loop
											getNextChar
											inspect
												ch
											when '0'..'9' then
												buffer.append_character (ch)
											when 'e', 'E' then -- not tested !!!
												if Efound then
													toRead := False
													toLeave := True
												else
													buffer.append_character ('E')
													Efound := True
												end -- if
											when '+', '-' then
												if Efound then
													-- one more bool flag is required ... to prevent 0.3E++--
													if EsignFound then
														toRead := False
														toLeave := True
													else
														EsignFound := True
														buffer.append_character (ch)
													end -- if
												else
													toRead := False
													toLeave := True
												end -- if
											else
												toRead := False
												toLeave := True
											end -- inspect
										end -- loop
									else
										token := integer_const_token
										integer_value := buffer.to_integer
										setTheNextToken (dot_token)
										toRead := False
									end
								else
									token := integer_const_token
									integer_value := buffer.to_integer									
									setTheNextToken (dot_token)
									toRead := False
								end
							else
								token := integer_const_token
								integer_value := buffer.to_integer
								toRead := False
							end
							toLeave := True
						end
					else
						setCharBuff (ch)
						token := integer_const_token
						integer_value := buffer.to_integer
						toRead := False
					end
				when '1' .. '9' then -- start of number constant
					scan_number_constant
				else
					buffer.append_character (ch)
					token := illegal_token
				end -- inspect
			end -- loop
			if pos > size and then token = no_token_assigned then
				tokenRow := row
				tokenCol := col
				setTokenNoRead(eof_token)
			end -- if
		end
--print("%Ttoken: " + tokenName(token) + "%N")
	end

	integer_value: Integer
	convertHexToDecimal is
	local
		i : Integer
		factor: Integer
	do
		from
			integer_value := 0
			factor := 1
			i := buffer.count
		until 
			i = 0
		loop
			inspect
				buffer.item (i)
			when '1' then
				integer_value := integer_value +  1 * factor
			when '2' then
				integer_value := integer_value +  2 * factor
			when '3' then
				integer_value := integer_value +  3 * factor
			when '4' then
				integer_value := integer_value +  4 * factor
			when '5' then
				integer_value := integer_value +  5 * factor
			when '6' then
				integer_value := integer_value +  6 * factor
			when '7' then
				integer_value := integer_value +  7 * factor
			when '8' then
				integer_value := integer_value +  8 * factor
			when '9' then
				integer_value := integer_value +  9 * factor
			when 'A', 'a' then
				integer_value := integer_value +  10 * factor
			when 'B', 'b' then
				integer_value := integer_value +  11 * factor
			when 'C', 'c' then
				integer_value := integer_value +  12 * factor
			when 'D', 'd' then
				integer_value := integer_value +  13 * factor
			when 'E', 'e' then
				integer_value := integer_value +  14 * factor
			when 'F', 'f' then
				integer_value := integer_value +  15 * factor
			end -- inspect
			i := i - 1
			factor := factor * 16
		end -- loop
	end -- 
	convertOctaToDecimal is
	local
		i : Integer
		factor: Integer
	do
		from
			integer_value := 0
			factor := 1
			i := buffer.count
		until 
			i = 0
		loop
			inspect
				buffer.item (i)
			when '1' then
				integer_value := integer_value +  1 * factor
			when '2' then
				integer_value := integer_value +  2 * factor
			when '3' then
				integer_value := integer_value +  3 * factor
			when '4' then
				integer_value := integer_value +  4 * factor
			when '5' then
				integer_value := integer_value +  5 * factor
			when '6' then
				integer_value := integer_value +  6 * factor
			when '7' then
				integer_value := integer_value +  7 * factor
			end -- inspect
			i := i - 1
			factor := factor * 8
		end -- loop
	end -- convertOctaToDecimal

	convertBitToDecimal is
	local
		i : Integer
		factor: Integer
	do
		from
			integer_value := 0
			factor := 1
			i := buffer.count
		until 
			i = 0
		loop
			if buffer.item (i) = '1' then
				integer_value := integer_value +  1 * factor
			end -- inspect
			i := i - 1
			factor := factor * 2
		end -- loop
	end -- if


	enableComments is
	do
		returnComments := True
	end -- enableComments

	disableComments is
	do
		returnComments := False
	end -- disableComments
	
feature {None}

	returnComments: Boolean
	
	scan_number_constant is 
	require
		current_char_is_digit: '0' <= ch and then ch <= '9'
	local
		isBitConst: Boolean
		isOctaConst: Boolean
		isHexConst: Boolean
		isRealConst: Boolean
		Efound: Boolean
		EsignFound: Boolean
		toLeave: Boolean
	do
		-- number.number E +/- number
		-- number ..
		-- number b/B/h/H/o/O
		isRealConst := False
		inspect
			ch
		when '0', '1' then
			isBitConst := True
		else
			isBitConst := False
		end -- inspect
		inspect	
			ch
		when '1'..'7' then
			isOctaConst := True
		else
			isOctaConst := False
		end -- inspect
		setCharBuff(ch)
		token := integer_const_token
		from
			toLeave := False
		until
			pos > size or else toLeave
		loop
			getNextChar
			inspect
				ch
			when '0', '1' then
				buffer.append_character (ch)
			when '2'..'7' then
				isBitConst := False
				buffer.append_character (ch)
			when '8'..'9' then
				isBitConst := False
				isOctaConst := False
				buffer.append_character (ch)
			when 'a', 'A', 'c', 'C', 'd', 'D', 'f', 'F' then
				isBitConst := False
				isOctaConst := False
				isHexConst := True
				buffer.append_character (ch)
			when 'b', 'B' then
				if isBitConst then
					isOctaConst := False
					isHexConst := False
				else
					toRead := False
				end -- if
				toLeave := True
			when 'o', 'O' then
				if isOctaConst then
					isBitConst := False
					isHexConst := False
				else
					toRead := False
				end -- if
				toLeave := True
			when 'h', 'H' then
				if isHexConst then
					isBitConst := False
					isOctaConst := False
				else
					toRead := False
				end -- if
				toLeave := True
			--------------------
			when 'e', 'E' then -- not tested !!! Incorrect !!!
				isBitConst := False
				isOctaConst := False
				if Efound then -- Incorrect!! check 5E5Eh
					toRead := False
					toLeave := True
				else
					buffer.append_character ('E')
					Efound := True
				end -- if
			when '+', '-' then
				if Efound then
					-- one more bool flag is required ... to prevent 0.3E++--
					if EsignFound then
						toRead := False
						toLeave := True
					else
						EsignFound := True
						buffer.append_character (ch)
					end -- if
				else
					toRead := False
					toLeave := True
				end -- if
			----------------------
			when '.' then -- Real or period or syntax error 
				if pos <= size then
					getNextChar
					inspect
						ch
					when '.' then
						setTheNextToken (period_token)
						toLeave := True
						toRead := True
					when '0'..'9' then -- fractional part
						isRealConst := True
						isBitConst := False
						isOctaConst := False
						isHexConst := False
						buffer.append_character (ch)
					else	
						setTheNextToken (dot_token)
						toRead := False
						toLeave := True
					end
				else -- decimal const
					isBitConst := False
					isOctaConst := False
					isHexConst := False
					setTheNextToken (dot_token)
					toLeave := True
				end -- if
			else
				isBitConst := False
				isOctaConst := False
				isHexConst := False
				toRead := False
				if isRealConst then
					token := real_const_token
				end
				toLeave := True
			end -- inspect
		end -- loop
		if token = integer_const_token then
			if isBitConst then
				convertBitToDecimal
			elseif isOctaConst then
				convertOctaToDecimal
			elseif isHexConst then
				convertHexToDecimal
			else
				integer_value := buffer.to_integer
			end -- if
		end -- if
	end -- scan_number_constant

	theNextToken: Integer
	theNextTokenRow: Integer
	theNextTokenCol: Integer
	
	setTheNextToken(t: Integer) is
	do
		theNextToken := t
		theNextTokenRow := tokenRow
		theNextTokenCol := tokenCol
	end -- setTheNextToken
	
	keywords: Array [String]
	file: File
	row, col, pos, size: Integer
feature {SlangCompiler, CompilationUnitCommon, SystemDescriptor}
	timeStamp: Integer
feature {None}
	toRead: Boolean
	buffer: String
	ch: Character
	returnSemicolon: Boolean

	next_row is
	require
		valid_row: row > 0
	do
		row := row + 1
		col := 0
	end -- next_row

	register_buffer_and_return_identifier_token: Integer is
	require
		pool_set: pool /= Void
	do
		buffer := pool.add_it (buffer)
		Result := identifier_token
	end -- register_buffer_and_return_identifier_token

	register_buffer_and_return_type_name_token: Integer is
	require
		pool_set: pool /= Void
	do
		buffer := pool.add_it (buffer)
		Result := type_name_token
	end -- register_buffer_and_return_type_name_token

	
	getKeywordPos: Integer is
	local
		first_ch: Character
		buff_len: Integer
	do
		first_ch := buffer.item (1)
		buff_len := buffer.count
		inspect
			first_ch
		when 'A' .. 'Z' then
			-- that is a unit name and a type!
			Result := register_buffer_and_return_type_name_token			    
		when 'a' then -- "alias", "as"
			inspect
				buff_len
			when 2 then
				if buffer.item (2) = 's' then
					Result := as_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 5 then
				if buffer.is_equal (keywords.item (alias_token)) then
					Result := alias_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			else
				Result := register_buffer_and_return_identifier_token
			end
		when 'b' then -- "build"
			if systemMode and then buffer.is_equal (keywords.item (build_token)) then
				Result := build_token
			else
				Result := register_buffer_and_return_identifier_token
			end -- if
		when 'c' then -- "concurrent", "const"
			inspect
				buff_len
			when 5 then
				if buffer.is_equal (keywords.item (const_token)) then
					Result := const_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 10 then
				if buffer.is_equal (keywords.item (concurrent_token)) then
					Result := concurrent_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			else
				Result := register_buffer_and_return_identifier_token
			end
		when 'd' then -- "do"
			if buff_len = 2 and then buffer.item (2) = 'o' then
				if Cmode then
					Result := illegal_token -- identifier_token
				else
					Result := do_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end
		when 'e' then -- "else" 4, "elsif" -5, "end" - 3, "ensure" 6, "extend" - 6, "elseif" - 6
			inspect
				buff_len
			when 3 then -- end
				if buffer.item (2) = 'n' and then buffer.item (3) = 'd' then
					if Cmode then
						Result := illegal_token -- identifier_token
					else
						Result := end_token
					end -- if
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 4 then -- else
				if buffer.is_equal (keywords.item (else_token)) then
					Result := else_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 5 then -- elsif
				if buffer.is_equal (keywords.item (elsif_token)) then
					Result := elsif_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 6 then -- "ensure", "extend", "elseif"
				if buffer.item (2) = 'n' then
					if buffer.is_equal (keywords.item (ensure_token)) then 
						Result := ensure_token
					else
						Result := register_buffer_and_return_identifier_token
					end -- if
				elseif buffer.item (2) = 'x' then
					if buffer.is_equal (keywords.item (extend_token)) then 
						Result := extend_token
					else
						Result := register_buffer_and_return_identifier_token
					end -- if
				elseif buffer.is_equal ("elseif") then
					Result := elsif_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			else
				Result := register_buffer_and_return_identifier_token
			end
		when 'f' then -- "final" - 5, "foreign" - 7
			inspect
				buff_len
			when 5 then
				if buffer.is_equal (keywords.item (final_token)) then
					Result := final_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			when 7 then
				if buffer.is_equal (keywords.item (foreign_token)) then
					Result := foreign_token
				else
					Result := register_buffer_and_return_identifier_token
				end
			else
				Result := register_buffer_and_return_identifier_token
			end
		when 'i' then -- "if", "in", "is"
			inspect
				buff_len
			when 2 then
				if buffer.item (2) = 's' then
					Result := is_token
				elseif buffer.item (2) = 'f' then
					Result := if_token
				elseif buffer.item (2) = 'n' then
					Result := in_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 'n' then -- "new", "none"
			inspect
				buff_len				
			when 3 then
				if buffer.item (2) = 'e' and then buffer.item (3) = 'w' then
					Result := new_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 4 then
				if buffer.is_equal (keywords.item (none_token)) then
					Result := none_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 'o' then -- "old", "override"
			inspect
				buff_len
			when 3 then
				if buffer.item (2) = 'l' and then buffer.item (3) = 'd' then
					Result := old_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 8 then
				if buffer.is_equal (keywords.item (override_token)) then
					Result := override_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 'p' then -- "pure"
			if buff_len = 4 and then buffer.item (2) = 'u' and then buffer.item (3) = 'r' and then buffer.item (4) = 'e' then
				Result := pure_token
			else
				Result := register_buffer_and_return_identifier_token
			end -- if
		when 'r' then -- "raise" - 5, "ref" - 3, "rename" - 6, "require" - 7, "return" - 6, "rigid" - 5, "rtn" - 3
			inspect
				buff_len
			when 3 then -- ref rtn
				if buffer.item (2) = 'e' and then buffer.item (3) = 'f' then
					Result := ref_token
				elseif buffer.item (2) = 't' and then buffer.item (3) = 'n' then
					Result := rtn_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 5 then -- raise rigid
				if buffer.is_equal (keywords.item (rigid_token)) then
					Result := rigid_token
				elseif buffer.is_equal (keywords.item (raise_token)) then
					Result := raise_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 6 then -- return
				if buffer.is_equal (keywords.item (return_token)) then
					Result := return_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 7 then -- "require"
				if buffer.is_equal (keywords.item (require_token)) then
					Result := require_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 's' then -- "safe", "select"
			inspect
				buff_len
			when 4 then
				if buffer.item (2) = 'a' and then buffer.item (3) = 'f' and then buffer.item (4) = 'e' then
					Result := safe_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 6 then
				if buffer.is_equal (keywords.item (select_token)) then
					Result := select_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end	-- inspect		
		when 't' then -- "this"
			if buff_len = 4 and then buffer.item (2) = 'h' and then buffer.item (3) = 'i' and then buffer.item (4) = 's' then
				Result := this_token
			else
				Result := register_buffer_and_return_identifier_token
			end -- if
		when 'u' then -- "unit", "use"
			inspect
				buff_len
			when 3 then
				if buffer.item (2) = 's' and then buffer.item (3) = 'e' then
					Result := use_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 4 then
				if buffer.item (2) = 'n' and then buffer.item (3) = 'i'  and then buffer.item (4) = 't' then
					Result := unit_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 'v' then -- "val", "var", "virtual"
			inspect
				buff_len
			when 3 then
				if buffer.item (2) = 'a' and then buffer.item (3) = 'r' then
					Result := var_token
				elseif buffer.item (2) = 'a' and then buffer.item (3) = 'l' then
					Result := val_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 7 then
				if buffer.is_equal (keywords.item (virtual_token)) then
					Result := virtual_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end -- inspect
		when 'w' then -- "when", "while"
			inspect
				buff_len
			when 4 then
				if buffer.item (2) = 'h' and then buffer.item (3) = 'e' and then buffer.item (4) = 'n' then
					Result := when_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			when 5 then
				if buffer.is_equal (keywords.item (while_token)) then
					Result := while_token
				else
					Result := register_buffer_and_return_identifier_token
				end -- if
			else
				Result := register_buffer_and_return_identifier_token
			end	-- inspect		
		else
			Result := register_buffer_and_return_identifier_token
		end -- inspect
	ensure
		token_assigned: Result > 0
	end
	
	getNextChar is
	require	
		not_eof: pos <= size
		position_within_file: pos > 0
		to_read_is_set: toRead
	do
		file.read_character
		ch  := file.last_character
		pos := pos + 1
		col := col + 1
	end -- getNextChar

	clear_buffer is
	require
		buffer_allocated: buffer /= Void
		non_empty_buffer: buffer.count > 0
	do
		buffer := ""
	ensure
		buffer_is_empty: buffer.count = 0
	end -- clear_buffer

	setCharBuff (aChar: Character) is
	require
		buffer_allocated: buffer /= Void
	do
		buffer := " "
		buffer.put (aChar, 1)
	end -- setCharBuff

	setToken (aToken: Integer) is
	--require
	--	keyword_token: alias_token <= aToken and then aToken <= one_line_function_token or else aToken = eof_token
	do	
		token := aToken
		if aToken >= alias_token then
			buffer := keywords.item (token)
		end -- if
	end -- setToken

	setTokenNoRead (aToken: Integer) is
	--require
	--	keyword_token: alias_token <= aToken and then aToken <= one_line_function_token or else aToken = eof_token
	do
		setToken (aToken)
		toRead := False
	end -- setTokenNoRead
	
	no_token_assigned: Integer is -1
	
end -- class SLang_Scanner
