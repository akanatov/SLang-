class SLang_SemAnal
create 
	init
feature {Any}
	validity_error (row, col : Integer; errorMessage: String) is
	do
		errorsCount := errorsCount + 1
		print ("Error at " + row.out + ":" + col.out + " - " + errorMessage + "%N")
	end -- validity_error
	validity_warning (row, col : Integer; errorMessage: String) is
	do
		print ("Warning at " + row.out + ":" + col.out + " - " + errorMessage + "%N")
	end -- validity_warning
	checkValidity (ast:CompilationUnitCompound) is
	-- To check validity of all elelments of the AST
	local
	do
	end -- checkValidity
	errorsCount: Integer
feature {None}
	init is
	do
	end
end -- class SLang_SemAnal