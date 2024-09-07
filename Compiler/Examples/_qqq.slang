 type  SLang_Scanner
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
		if token is
		:integer_const_token 
			return integer_value
		:real_const_token 
			return buffer.to_real
		:string_const_token 
			return buffer 
		:char_const_token
			return buffer (1)
		else
		end // if		
	ensure
		return is Any implies (
			token = integer_const_token or else 
			token = real_const_token or else
			token = string_const_token or else
			token = char_const_token
		)
	end // tokenValue
end