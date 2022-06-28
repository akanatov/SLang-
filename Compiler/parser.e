class SLang_Parser
create
	init
feature {None}
trace (message: String ) is
do
	o.putNL ("At: " + scanner.tokenRow.out + ":" + scanner.tokenCol.out + " - "+ scanner.token.out + "`" + scanner.tokenName (scanner.token) + "`: " + message)
end -- trace
	
not_implemented_yet (featureName: String) is
require
	feature_name_not_void : featureName /= Void
do
	errorsCount := errorsCount + 1
	o.putNL ("NOT IMPLEMENTED YET @" + scanner.tokenRow.out + ":" + scanner.tokenCol.out + ":`" + scanner.tokenName (scanner.token) + "`<" + featureName + ">")
end -- not_implemented_yet

	o: Output

feature {None}
	validToken (tokens: Array [Integer]): Boolean is
	require
		non_void_tokens: tokens /= Void
	local
		i, n: Integer
	do
		from
			n := tokens.count
		until
			i > n
		loop
			if scanner.token = tokens.item (i) then
				Result := True	
				i := n + 1
			else
				i := i + 1
			end -- if
		end -- loop
	end -- validToken

	setSourcePosition (node: SourcePosition) is
	do
		node.set_rc (scanner.tokenRow, scanner.tokenCol)
	end -- setSourcePosition
	
feature {Any}

	errorsCount: Integer
	warningsCount: Integer
	
	systems: Sorted_Array [SystemDescriptor]

	--ast: CompilationUnitCommon
	ast: CompilationUnitCompound
	
	--unitAST: CompilationUnitUnit
	--scriptAST: CompilationUnitAnonymousRoutine
	--routinesAST: CompilationUnitStandaloneRoutines

	parseSourceFile is
	-- parse Slang source file which can optionally start with the system description
	local
		name: String
		toExit: Boolean
		stmtDsc: StatementDescriptor
		rtnDsc: StandaloneRoutineDescriptor
		identDsc: IdentifierDescriptor
		exprDsc: ExpressionDescriptor
		uCallDsc: UnqualifiedCallDescriptor
		sysDsc: SystemDescriptor
	do
		from 
			scanner.enableSystemMode
			scanner.nextToken
		until
			toExit
		loop
			inspect
				scanner.token
			when scanner.illegal_token then
				syntaxError ("Compilation unit start expected", <<scanner.illegal_token>>, unit_folowers)
				toExit := True
			when scanner.eof_token then
				toExit := True
			when scanner.build_token then
				sysDsc := parseSystemDescription
				if sysDsc /= Void and then not systems.added (sysDsc) then
					validity_warning ( "Duplicated system declaration `" + sysDsc.name + "', ignored")
				end -- if
			else
				scanner.disableSystemMode
debug
--	trace ("Start .... ")
end
				inspect
					scanner.token
				when scanner.use_token then
					-- parse use_const clause
					parseUseClause
				-- Unit start: ([final] [ref|val|concurrent])|[virtual]|[extend]
				when scanner.final_token then -- parse final unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.ref_token then -- parse ref unit
						scanner.nextToken
						inspect	
							scanner.token
						when scanner.unit_token then -- parse unit
							-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
							parseUnit (True, True, False, False, False, False)
						else
							syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
							toExit := True
						end
					when scanner.val_token then -- parse val unit
						scanner.nextToken
						inspect	
							scanner.token
						when scanner.unit_token then -- parse unit
							-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
							parseUnit (True, False, True, False, False, False)
						else
							syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
						end
					when scanner.concurrent_token then -- parse concurrent unit
						scanner.nextToken
						inspect	
							scanner.token
						when scanner.unit_token then -- parse unit
							-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
							parseUnit (True, False, False, True, False, False)
						else
							syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
						end
					when scanner.unit_token then -- parse unit
						scanner.nextToken
						inspect	
							scanner.token
						when scanner.type_name_token then -- parse unit
							-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
							parseUnit (True, False, False, False, False, False)
						else
							syntaxError ("Unit name expected", <<scanner.type_name_token>>,<<>>)
						end
					else
						syntaxError ("Unit start expected", <<scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.unit_token>>, unit_folowers)
					end -- inspect
					ast.stop_unit_parsing
				when scanner.ref_token then -- parse ref unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.unit_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, True, False, False, False, False)
					else
						syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
					end
					ast.stop_unit_parsing
				when scanner.val_token then -- parse val unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.unit_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, False, True, False, False, False)
					else
						syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
					end
					ast.stop_unit_parsing
				when scanner.concurrent_token then -- parse concurrent unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.unit_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, False, False, True, False, False)
					else
						syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
					end
					ast.stop_unit_parsing
				when scanner.virtual_token then -- parse virtual unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.unit_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, False, False, False, True, False)
					else
						syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
					end
					ast.stop_unit_parsing
				when scanner.extend_token then -- parse extend unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.unit_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, False, False, False, False, True)
					else
						syntaxError ("Unit start expected", <<scanner.unit_token>>, unit_folowers)
					end
					ast.stop_unit_parsing
				when scanner.unit_token then -- parse unit
					ast.start_unit_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.type_name_token then -- parse unit
						-- is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend
						parseUnit (False, False, False, False, False, False)
					else
						syntax_error (<<scanner.type_name_token>>)
						toExit := True
					end
					ast.stop_unit_parsing
				when scanner.pure_token then -- start of standalone pure routine
					ast.start_standalone_routine_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.identifier_token then -- standalone pure routine
						rtnDsc := parseStandAloneRoutine (True, False)
						if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
							validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
						end -- if
					else
						syntax_error (<<scanner.identifier_token>>)
						toExit := True
					end
					ast.stop_standalone_routine_parsing
				when scanner.safe_token then -- start of standalone safe routine
					ast.start_standalone_routine_parsing
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.identifier_token then -- standalone safe routine
						rtnDsc := parseStandAloneRoutine (False, True)
						if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
							validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
						end -- if
					else
						syntax_error (<<scanner.identifier_token>>)
						toExit := True
					end
					ast.stop_standalone_routine_parsing
				when scanner.identifier_token then -- start of standalone routine or statement of the anonymous one
-- Not sure what to do with pools !!!!
					name := scanner.tokenString
					create identDsc.init (name) -- scanner.tokenString)
					ast.add_name_to_standalone_routines_pool (name)
					scanner.nextToken
					inspect	
						scanner.token
					when scanner.colon_token, scanner.implies_token then
						-- ident: function defintion or local attribute !!!
						parseFunctionOrLocalAttribute (name)
					when scanner.left_paranthesis_token then
						-- identifier ( .... Huh .... prase further ...
						parseAssignmentOrUnqualifiedCallOrRoutineStart (name)
					when scanner.dot_token then
						-- Anonymous routine call statement : ident.
						stmtDsc := parseCallWithOptionalTailAssignment (identDsc)
						if stmtDsc /= Void then
							ast.addStatement (stmtDsc)
						end -- if
					when scanner.assignment_token then
						-- Anonymous routine assignemnt: ident := 
						stmtDsc := parseAssignmentToIdentifierStatement (name)
						if stmtDsc /= Void then
							ast.addStatement (stmtDsc)
						end -- if				
					when scanner.is_token then 
						-- Anonymous routine: ident is <Expression>
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
--							create {AssignmentStatementDescriptor} stmtDsc.init (identDsc, exprDsc)
							create {EntityDeclarationStatementDescriptor} stmtDsc.init (False, False, False, name, exprDsc)
							if stmtDsc /= Void then
								ast.addStatement (stmtDsc)
							end -- if				
						end -- if
					when -- scanner.final_token, scanner.alias_token, 
						scanner.require_token, scanner.one_line_function_token,
						scanner.use_token, scanner.foreign_token -- , scanner.implies_token
					then
						-- Standalone routine start
--trace (">>#6")
						ast.start_standalone_routine_parsing
						rtnDsc := parseStandAloneRoutine1 (False, False, name, Void)
						if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
							validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
						end -- if					
						ast.stop_standalone_routine_parsing
					else
						--if scanner.Cmode and then (scanner.token = scanner.less_token or else scanner.token = scanner.left_curly_bracket_token)
						--	or else (scanner.token = scanner.left_square_bracket_token or else scanner.token = scanner.do_token)
						--then
						if scanner.blockStart then
							-- Standalone routine start
--trace (">>#5")
							ast.start_standalone_routine_parsing
							rtnDsc := parseStandAloneRoutine1 (False, False, name, Void)
							if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
								validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
							end -- if					
							ast.stop_standalone_routine_parsing
						elseif scanner.genericsStart then
--trace (">>#5.5")
-- NOT SUPPORTED foo[T]() - call chain !!!
-- Parse generics  either declaration or instantiation foo [G] vs foo [Type] vs foo [expr] vs foo [G extend Type] vs foo [G new ...]
-- require|do|foreign|none - stanaloen routine
-- foo[Integer] - valid procedure call !!!
-- foo [Integer] (arguments)
-- Headache to parse !!!
							ast.start_standalone_routine_parsing
							rtnDsc := parseStandAloneRoutine1 (False, False, name, Void)
							if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
								validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
							end -- if					
							ast.stop_standalone_routine_parsing
						elseif scanner.Cmode then
							syntax_error (<<
								-- scanner.final_token, scanner.alias_token,
								scanner.left_paranthesis_token, scanner.colon_token,
								scanner.left_curly_bracket_token,
								scanner.require_token, scanner.one_line_function_token, scanner.less_token, scanner.dot_token, 
								scanner.assignment_token, scanner.use_token, scanner.foreign_token, scanner.is_token
							>>)
							toExit := True
						else
							-- Let's assume that was a procedure call in the anonymous routine and continue parsing !!!
							create uCallDsc.init (identDsc, Void, Void)
							ast.addStatement (uCallDsc)

							--syntax_error (<<
							--	-- scanner.final_token, scanner.alias_token, 
							--	scanner.left_paranthesis_token, scanner.colon_token, scanner.do_token,
							--	scanner.require_token, scanner.one_line_function_token, scanner.left_square_bracket_token, scanner.dot_token, 
							--	scanner.assignment_token, scanner.use_token, scanner.foreign_token, scanner.is_token
							-->>)
							--toExit := True
						end -- if
					end -- inspect
				-- Statement: Assignment | LocalAttributeCreation | +IfCase | ? Identifier | Return | +HyperBlock | Raise |MemberCallOrCreation 		|  +Loop
				--            ( ident      var ident                if        ?              return    +require do   raise   ident new old this return     while require do
				-- Anonymous routine start (statements list)
				when scanner.type_name_token then -- Anonymous routine: Module feature call
					stmtDsc := parseModuleCall
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if
				when scanner.if_token then -- Anonymous routine: if statement
					stmtDsc := parseIfStatement
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if
				when scanner.while_token then -- Anonymous routine: loop statement
					stmtDsc := parseWhileStatement (False)
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if				
				when scanner.new_token then -- Anonymous routine: new statement
					stmtDsc := parseNewStatement
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if				
				when scanner.detach_token then -- Anonymous routine:  ? statement
					stmtDsc := parseDetachStatement
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if				
				when scanner.raise_token then -- Anonymous routine: raise statement
					stmtDsc := parseRaiseStatement
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if				
				when scanner.return_token then -- Anonymous routine: return statement
					stmtDsc := parseReturnStatement
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if
				when scanner.left_paranthesis_token then -- Anonymous routine: turple assignment or call (a, b) := expr or ().foo ...
					stmtDsc := parseAssignmentOrQualifiedCall
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if
				when scanner.var_token, scanner.rigid_token then -- Anonymous routine - local attribute declaration
					-- all locals are const by default
					parseLocalsDeclaration (ast.statements, True, Void)
				when scanner.require_token then -- Anonymous routine: block/loop statement
					stmtDsc := parseBlock (scanner.token)
					if stmtDsc /= Void then
						ast.addStatement (stmtDsc)
					end -- if				
				else
					if scanner.blockStart then  -- Anonymous routine: block/loop statement
						stmtDsc := parseBlock (scanner.token)
						if stmtDsc /= Void then
							ast.addStatement (stmtDsc)
						end -- if				
					else
						syntaxError ("Start of compilation unit expected", unit_folowers, <<>>)
					end -- if
				end -- inspect
			end -- inspect
			if not toExit then
				scanner.enableSystemMode
			end -- if			
		end -- loop
	end -- parseSourceFile
	
feature {None}

	parseCallWithOptionalTailAssignment (targetDsc: ExpressionDescriptor): StatementDescriptor is
	require
		non_void_target_expression: targetDsc /= Void
		valid_token: validToken (<<scanner.dot_token, scanner.left_paranthesis_token>>)
	local
		exprDsc: ExpressionDescriptor
		callDsc: CallDescriptor	
	do
		callDsc := parseWritableCall (targetDsc)
		if callDsc /= Void then
			inspect
				scanner.token
			when scanner.assignment_token then
				-- <writable> := <expr>
				scanner.nextToken
				exprDsc := parseExpression
				if exprDsc /= Void then
					create {AssignmentStatementDescriptor} Result.init (callDsc, exprDsc)
				end -- if
			else
				Result := callDsc
			end -- if
		end -- if				
	end -- parseCallWithOptionalTailAssignment

	unit_folowers: Array [Integer] is
	do
		Result := <<
			 scanner.build_token, scanner.use_token, scanner.final_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token,
			 scanner.virtual_token, scanner.extend_token, scanner.unit_token, scanner.pure_token, scanner.safe_token, scanner.identifier_token,
			 scanner.type_name_token,
			 scanner.if_token, scanner.while_token, scanner.new_token, scanner.detach_token, scanner.raise_token, scanner.return_token,
			 scanner.left_paranthesis_token, scanner.var_token, scanner.require_token, scanner.rigid_token
		>>
		if scanner.Cmode then			
			Result.force (scanner.left_curly_bracket_token, Result.count + 1)
		else
			Result.force (scanner.do_token, Result.count + 1)
		end -- if
	end -- unit_folowers

	parseSystemDescription: SystemDescriptor is
	require
		valid_token: validToken (<<scanner.build_token>>)
	local 
		name: String
		entry: String
		paths: Sorted_Array [String]
		clusters: Sorted_Array [ClusterDescriptor] -- clusters with adaptations
		libraries: Sorted_Array [String] -- object/lib/dll imp files to link with the system
		wasError: Boolean
		src_pos: expanded SourcePosition		
	do
-- trace (">>parseSystemDescription")
		scanner.nextToken
		inspect
			scanner.token
		when scanner.identifier_token, scanner.type_name_token, scanner.string_const_token then
			name := scanner.tokenString
			src_pos := scanner.source_position
			scanner.nextToken
		else
			syntax_error (<<scanner.identifier_token, scanner.string_const_token>>)
			wasError := True
		end -- inspect
		if not wasError then
-- trace ("%TparseSystemDescription: " + name)
			inspect
				scanner.token
			when scanner.colon_token then
				-- Library
				scanner.nextToken
				paths := parseStrings
			when scanner.one_line_function_token then
				-- Program
				scanner.nextToken
				inspect
					scanner.token
				when scanner.identifier_token, scanner.type_name_token then
					entry := scanner.tokenString
					scanner.nextToken
				else
					syntax_error (<<scanner.identifier_token>>)
					wasError := True
				end -- inspect
			else
				syntax_error (<<scanner.colon_token, scanner.one_line_function_token>>)
				wasError := True
			end -- inspect
			if not wasError then
				if scanner.token = scanner.use_token then
					scanner.nextToken
					clusters := parseClusters
				end -- if
				if scanner.token = scanner.foreign_token then
					scanner.nextToken
					libraries:= parseStrings
				end -- if
				if scanner.token = scanner.end_token then
					scanner.nextToken
					if entry = Void then
						create Result.init_library (name, paths, clusters, libraries)
					else
						create Result.init_program (name, entry, clusters, libraries)
					end -- if
					Result.setSourcePosition (src_pos)
				else
					syntax_error (<<scanner.end_token>>)
				end -- if
			end -- if
		end -- if 
-- trace ("<<parseSystemDescription")
	end -- parseSystemDescription

	parseClusters: Sorted_Array [ClusterDescriptor] is
	local
		name: String
		clusterDsc: ClusterDescriptor
		exclude_clause: Sorted_Array [String]
		rename_clause: Sorted_Array [RenamePair]
		select_clause: Sorted_Array [String]
		toLeave: Boolean
		wasError: Boolean
	do
		from
			create Result.make
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.identifier_token, scanner.type_name_token, scanner.string_const_token then
				name := scanner.tokenString
				scanner.nextToken
				if scanner.token = scanner.tilda_token then
					-- parse exclude clause
					scanner.nextToken
					exclude_clause := parseStrings
				end -- if
				if scanner.token = scanner.implies_token then
					-- parse rename clause
					scanner.nextToken
					rename_clause := parse_rename_clause
				end -- if
				if scanner.token = scanner.select_token then
					-- parse select clause
					scanner.nextToken
					select_clause := parseStrings
				end -- if
				create clusterDsc.init (name, exclude_clause, rename_clause, select_clause)
				if not Result.added (clusterDsc) then
					validity_error ( "Duplicated cluster `" + name + "` identified")
					wasError := True
				end -- if
			else
				toLeave := True
			end -- inspect
		end -- loop
		if Result.count = 0 or else wasError then
			Result := Void
		end -- if
	end -- parseClusters
	
	
	parse_rename_clause: Sorted_Array [RenamePair] is
	local
		oldName, newName: String
		rnmPair: RenamePair
		toLeave: Boolean
		wasError: Boolean
	do
		from
			create Result.make
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.identifier_token, scanner.type_name_token, scanner.string_const_token then
				oldName := scanner.tokenString
				scanner.nextToken
				if scanner.token = scanner.as_token then
					scanner.nextToken
					inspect
						scanner.token
					when scanner.identifier_token, scanner.type_name_token, scanner.string_const_token then
						newName := scanner.tokenString
						scanner.nextToken
						create rnmPair.init (oldName, newName)
						if not Result.added (rnmPair) then
							validity_warning ( "Duplicated rename `" + rnmPair.out + "', ignored")
						end -- if
					else
						syntax_error (<<scanner.identifier_token, scanner.string_const_token>>)
						wasError := True
						toLeave := True
					end -- if
				else
					syntax_error (<<scanner.as_token>>)
					wasError := True
					toLeave := True
				end -- if
			else
				toLeave := True
			end -- inspect
		end -- loop
		if Result.count = 0 or else wasError then
			Result := Void
		end -- if
	end -- parse_rename_clause
	
	parseStrings: Sorted_Array [String] is
	local
		name: String
		toLeave: Boolean
	do
		from
			create Result.make
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.identifier_token, scanner.type_name_token, scanner.string_const_token then
				name := scanner.tokenString
				if not Result.added (name) then
					validity_warning ( "Duplicated name `" + name + "', ignored")
				end -- if
				scanner.nextToken
			else
				toLeave := True
			end -- inspect
		end -- loop
		if Result.count = 0 then
			Result := Void
		end -- if
	end -- parseStrings
	
	parseFunctionOrLocalAttribute (name: String) is
		-- ident: function defintion or local attribute !!!
	require
		valid_token: validToken (<<scanner.colon_token, scanner.implies_token>>)
		name_not_void: name /= Void
	local
		type: TypeDescriptor
		detDsc: DetachableTypeDescriptor
		attDsc: AttachedTypeDescriptor
		localDsc: LocalAttrDeclarationDescriptor
		expr: ExpressionDescriptor
		rtnDsc: StandaloneRoutineDescriptor
		isFunction: Boolean
	do
		isFunction := scanner.token = scanner.implies_token
		scanner.nextToken
debug
	--trace ("parseFunctionOrLocalAttribute " + name + ": ")
end -- debug
		type := parseTypeDescriptor
		if type = Void then
debug
	--trace ("parseFunctionOrLocalAttribute " + name + ": (Void) !!!!")
end -- debug
			--validity_error( "Type of `" + name + "` is not recognized which starts from " +  scanner.tokenString) -- + " in file `" + scanner.sourceFileName + "`")
			--scanner.nextToken			
		elseif isFunction then
			inspect
				scanner.token
			when scanner.require_token, scanner.foreign_token, scanner.use_token then -- , scanner.none_token
				-- function
debug
--trace (">>#4")
end -- debug
				ast.start_standalone_routine_parsing
				rtnDsc := parseStandAloneRoutine1 (False, False, name, type)
				if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
					validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
				end -- if					
				ast.stop_standalone_routine_parsing
			else
				if scanner.blockStart then
					-- function
debug
--trace (">>#3")
end -- debug
					ast.start_standalone_routine_parsing
					rtnDsc := parseStandAloneRoutine1 (False, False, name, type)
					if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
						validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
					end -- if					
					ast.stop_standalone_routine_parsing
				elseif scanner.Cmode then
					syntax_error (<<
						scanner.require_token, scanner.foreign_token, scanner.use_token,
						scanner.left_curly_bracket_token
					>>)
				else
					syntax_error (<<
						scanner.require_token, scanner.foreign_token, scanner.use_token,
						scanner.do_token
					>>)			
				end -- if
			end -- inspect			
		else
--trace (">>> parseFunctionOrLocalAttribute - " + name + ": " + type.out)
			inspect
				scanner.token
			when scanner.is_token then
				-- ident: type is expr // initialization
				scanner.nextToken
				expr := parseExpressionWithSemicolon
				if expr /= Void then
					detDsc ?= type
					if detDsc = Void then
						attDsc ?= type
						check
							valid_attached_type: attDsc /= Void
						end -- check
						create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, name, attDsc, expr)
					else
						create {DetachedLocalAttributeDeclarationDescriptor} localDsc.init (name, detDsc) --.type)
					end -- if
					if not ast.addedLocalDeclarationStatement (localDsc) then
						validity_error( "Duplicated local declaration `" + localDsc.name + "`") 
					end -- if
				end -- if
			when scanner.require_token, scanner.foreign_token, scanner.use_token then -- , scanner.none_token
				-- function
--trace (">>#4")
				ast.start_standalone_routine_parsing
				rtnDsc := parseStandAloneRoutine1 (False, False, name, type)
				if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
					validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
				end -- if					
				ast.stop_standalone_routine_parsing
			else
				if scanner.blockStart then
					-- function
--trace (">>#3")
					ast.start_standalone_routine_parsing
					rtnDsc := parseStandAloneRoutine1 (False, False, name, type)
					if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
						validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
					end -- if					
					ast.stop_standalone_routine_parsing
				else
					-- ident: ?type
					detDsc ?= type
					if detDsc = Void then
						attDsc ?= type
						check
							valid_attached_type: attDsc /= Void
						end -- check
						create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, name, attDsc, Void)
						if not ast.addedLocalDeclarationStatement (localDsc) then
							validity_error( "Duplicated local declaration `" + localDsc.name + "`") 
						end -- if
					
						---- attribute initialization is missed
						--validity_error( "For attribute `" + name + ": " + type.out + "` initialization is missed") 
					else
						create {DetachedLocalAttributeDeclarationDescriptor} localDsc.init (name, detDsc) --.type)
						if not ast.addedLocalDeclarationStatement (localDsc) then
							validity_error( "Duplicated local declaration `" + localDsc.name + "`") 
						end -- if
					end -- if
				end -- if
			end -- inspect			
		end -- if
	end -- parseFunctionOrLocalAttribute

	parseUnqalifiedCallWithFirstArgument (name: String): UnqualifiedCallDescriptor is
	-- name ( x		>> unqualified call or assignment ().x or ().x := expr
	--        ^ 
	require
		target_not_void: name /= Void
		valid_token: validToken (<<
			scanner.operator_token,
			scanner.bar_token, scanner.tilda_token, scanner.identifier_token, scanner.type_name_token
		>>)
	local
		callChain: Array [CallChainElement]
		arguments: Array [ExpressionDescriptor]
		identDsc: IdentifierDescriptor
	do
		create identDsc.init (name)
		arguments := parseArguments1
		callChain := parseCallChain
		create {UnqualifiedCallDescriptor}Result.init (identDsc, arguments, callChain)
	end -- parseUnqalifiedCallWithFirstArgument


	parseLocalsDeclaration(statements: Array [StatementDescriptor]; isVarOrRigid: Boolean; name: String) is
	require
		statements_not_void: statements /= Void
	local
		locals: Sorted_Array [LocalAttrDeclarationDescriptor]
	do
--trace ("parseLocalsDeclaration")
		locals := parseLocalAttributesDeclaration (isVarOrRigid, name)
		if locals /= Void then
			statements.append (locals)
		end -- if
	end -- parseLocalsDeclaration

	parseAssignmentOrUnqualifiedCallOrRoutineStart (name: String) is
	-- name (var 			>> routine declaration
	-- name ()				>> unqualified call or routine declaration
	-- name (operator		>> unqualified call or assignment ().x or ().x := expr
	-- name (id: 			>> routine declaration
	-- name (id.			>> unqualified call or assignment
	-- name (id operator 	>> unqualified call or assignment
	-- name ( nextName ) 	>> unqualified call or assignment
	-- name (id, id, ... 	>> scan further
	-- 		name (id,..., var			>> routine declaration
	-- 		name (id,..., id:			>> routine declaration
	-- 		name (id,..., id.	 		>> unqualified call or assignment
	-- 		name (id,..., id operator	>> unqualified call or assignment
	-- 		name (id,..., operator		>> unqualified call or assignment
	require
		name_not_void: name /= Void
		valid_token: validToken (<<scanner.left_paranthesis_token>>)
	local	
		nextName: String
		toLeave: Boolean
		parameters: Array [ParameterDescriptor]
		rtnDsc: StandaloneRoutineDescriptor
		callDsc: UnqualifiedCallDescriptor
		exprDsc: ExpressionDescriptor
		asgnDsc: AssignmentStatementDescriptor
		identDsc: IdentifierDescriptor
		skipParametersParsing: Boolean
		statementProcessed: Boolean
		commaFound: Boolean
		wasError: Boolean
		isRtnDecl: Boolean
	do
--trace (">>>parseAssignmentOrUnqualifiedCallOrRoutineStart")
		scanner.nextToken
		inspect	
			scanner.token 
		when scanner.rigid_token then
			-- name (rigid 			>> routine declaration
			-- identifier ( rigid --> routine declaration
			--              ^
			-- Identifier Parameters [“:” Type] [EnclosedUseDirective] [RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | ( foreign| (“=>”Expression ) [EnsureBlock end] )
			isRtnDecl := True
		when scanner.operator_token, scanner.implies_token, scanner.bar_token, scanner.tilda_token then
			-- name ( operator		>> unqualified call or assignment ().x or ().x := expr
			--        ^
		when scanner.right_paranthesis_token then 
			-- name ( )		>> unqualified call or routine declaration
			--        ^
			scanner.nextToken
			inspect	
				scanner.token 
			when scanner.dot_token then
				-- name (). >> call or assignemnt
			else
				if scanner.Cmode then			
					if scanner.token = scanner.left_curly_bracket_token then
						isRtnDecl := True
						skipParametersParsing := True
					else
						statementProcessed := True
					end -- if
				else
					if scanner.token = scanner.do_token then
						isRtnDecl := True
						skipParametersParsing := True
					else
						statementProcessed := True
					end -- if
				end -- if			
				if statementProcessed then
					create identDsc.init (name)
					create {UnqualifiedCallDescriptor}callDsc.init (identDsc, Void, Void)
					if scanner.token = scanner.assignment_token then
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							create asgnDsc.init (callDsc, exprDsc)
							ast.statements.force (asgnDsc, ast.statements.count + 1)
						end -- if
					else
						ast.statements.force (callDsc, ast.statements.count + 1)
					end -- if
				end -- if
			end -- inspect
		when scanner.type_name_token then
			-- name ( TypeName  >> that is call or assignemnt
			--        ^
		when scanner.identifier_token then
			-- name ( nextName
			--        ^
			scanner.push
			nextName := scanner.tokenString
			scanner.nextToken
			scanner.push
			inspect	
				scanner.token 
			when scanner.right_paranthesis_token then
				-- name ( nextName ) [{CallChain} | operator Expr] -- unqualified call!!!
				--                 ^
				--        ^
				scanner.revert
			when scanner.colon_token, scanner.is_token then
				-- name (id: 			>> routine declaration
				-- ident ( ident :
				-- ident ( ident is
				--               ^
				--         ^
				-- revert to ident!!! 
				scanner.revert
				isRtnDecl := True
			when scanner.dot_token then
				-- name ( id.			>> unqualified call or assignment
				--          ^
				--        ^ 
				scanner.revert
			when scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.bar_token, scanner.tilda_token, scanner.less_token, scanner.greater_token
			then
				-- name ( id operator 	>> unqualified call or assignment
				--           ^
				--        ^
				scanner.revert
			when scanner.comma_token then
				--	name (id,..., rigid			>> routine declaration
				-- 	name (id,..., id:			>> routine declaration
				-- 	name (id,..., id.	 		>> unqualified call or assignment
				-- 	name (id,..., id operator	>> unqualified call or assignment
				-- 	name (id,..., operator		>> unqualified call or assignment
				--	name (id,..., id ) 			>> unqualified call or assignment
				--          ^
				from
					commaFound := True
					scanner.nextToken
				until
					toLeave
				loop
					inspect
						scanner.token
					when scanner.eof_token then
						scanner.flush
						validity_error( "Unexpected end of file `" + scanner.sourceFileName + "`")
						toLeave := True
						wasError := True
					when scanner.operator_token, -- scanner.minus_token,
						scanner.implies_token, scanner.bar_token, scanner.tilda_token
					then
						-- 	name (id,..., operator		>> unqualified call or assignment
						--				  ^
--trace ("operator found reverting ...")
						scanner.push
						scanner.revert
						toLeave := True
--trace ("operator found reverted !!!")
					when scanner.rigid_token then
						--	name (id,..., rigid			>> routine declaration
						--				  ^
						scanner.push
						scanner.revert
						toLeave := True
						isRtnDecl := True
					when scanner.identifier_token then 
						if commaFound then
							commaFound := False
							scanner.push
							scanner.nextToken
							inspect
								scanner.token
							when scanner.colon_token then
								-- 	name (id,..., id:			>> routine declaration
								--					^
								scanner.push
								scanner.revert
								toLeave := True
								isRtnDecl := True
							when scanner.dot_token then
								-- 	name (id,..., id.	 		>> unqualified call or assignment
								--					^
								scanner.push
								scanner.revert
								toLeave := True
							when scanner.operator_token, -- scanner.minus_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
							then
								-- 	name (id,..., id operator	>> unqualified call or assignment
								--					 ^
								scanner.push
								scanner.revert
								toLeave := True
							else
								-- continue
--trace ("Next ...")
							end -- inspect
						else
							syntax_error (<<
								scanner.comma_token, scanner.right_paranthesis_token, scanner.operator_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token,
								scanner.bar_token, scanner.tilda_token, scanner.colon_token
							>>)
							scanner.flush
							toLeave := True
							wasError := True
						end -- if
					when scanner.comma_token then 
						if commaFound then
							syntax_error (<<
								scanner.rigid_token, scanner.identifier_token, scanner.operator_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token,
								scanner.bar_token, scanner.tilda_token
							>>)
							scanner.flush
							toLeave := True
							wasError := True
						else
							scanner.push
							commaFound := True
							scanner.nextToken
--trace ("Comma found")
						end -- if
					when scanner.right_paranthesis_token then 
						if commaFound then
							syntax_error (<<
								scanner.rigid_token, scanner.identifier_token, scanner.operator_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token,
								scanner.bar_token, scanner.tilda_token
							>>)
							scanner.flush
							toLeave := True
							wasError := True
						else
							-- Unqalified call: ident (ident, ident, ... ident)
							scanner.revert
							toLeave := True
						end -- if
					else
						syntax_error (<<
							scanner.operator_token,
							scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token, 
							scanner.rigid_token, scanner.identifier_token, scanner.comma_token,
							scanner.right_paranthesis_token
						>>)
						scanner.flush
						toLeave := True
						wasError := True
					end -- inspect
				end -- loop
			else
				syntax_error (<<
					scanner.colon_token, scanner.operator_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.dot_token, scanner.right_paranthesis_token,
					scanner.is_token
				>>)
				scanner.flush				
				wasError := True
			end -- inspect			
		else
			syntax_error (<<
				scanner.rigid_token, scanner.operator_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.identifier_token
			>>)
			wasError := True
		end -- inspect
		if not wasError then
			if isRtnDecl then
				-- >> routine declaration
-- trace ("Parsing analysis done OK: >> routine declaration")
				ast.start_standalone_routine_parsing
				if skipParametersParsing then
					rtnDsc ?= parseAnyRoutine (False, False, False, False, name, Void, Void, True, False, Void, False)
					if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
						validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
					end -- if					
				else
					parameters := parseParameters1 (True)
					if parameters /= Void then
						rtnDsc ?= parseAnyRoutine (False, False, False, False, name, Void, Void, True, False, parameters, False)			
						if rtnDsc /= Void and then not ast.routines.added (rtnDsc) then
							validity_error( "Duplicated routine declaration `" + rtnDsc.name + "`") 
						end -- if					
					end -- if
				end -- if
				ast.stop_standalone_routine_parsing
			elseif not statementProcessed then
				-- >> unqualified call or assignment
--trace ("Parsing analysis done OK: >> >> unqualified call or assignment")
				callDsc := parseUnqalifiedCallWithFirstArgument (name)
				if callDsc /= Void then
					if scanner.token = scanner.assignment_token then
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							create asgnDsc.init (callDsc, exprDsc)
							ast.statements.force (asgnDsc, ast.statements.count + 1)
						end -- if
					else
						ast.statements.force (callDsc, ast.statements.count + 1)
					end -- if
				end -- if
			end -- if
		end -- if		
	end -- parseAssignmentOrUnqualifiedCallOrRoutineStart

	parseTupleExpression (firstExprDsc: ExpressionDescriptor): ExpressionDescriptor is
	require
		valid_token: validToken (<<scanner.comma_token>>)
	local
		expressions: Array [ExpressionDescriptor]
		exprDsc: ExpressionDescriptor
		callChain: Array [CallChainElement]
		toLeave: Boolean
		wasError: Boolean
	do
		from
			expressions := <<firstExprDsc>>
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.comma_token then
				scanner.nextToken
				exprDsc := parseExpression
				if exprDsc = Void then
					toLeave := True
					wasError := True
				else
					expressions.force (exprDsc, expressions.count + 1)
				end -- if
			when scanner.right_paranthesis_token then
				scanner.nextToken
				toLeave := True
			else
				syntax_error (<<scanner.comma_token, scanner.right_paranthesis_token>>)
				toLeave := True
				wasError := True
			end -- inpsect
		end -- loop
		if not wasError then
			if expressions.count = 1 then
				create {ParenthedExpressionDescriptor} Result.init (firstExprDsc)
			else
				create {TupleExpressionDescriptor} Result.init (expressions)
			end -- if
			if scanner.token = scanner.dot_token then
				-- “(”Expression {“,”Expression}“)” “.”Identifier [ Arguments ] {CallChain}
				--                                   ^
				callChain := parseCallChain
				if callChain /= Void then
					create {ExpressionCallDescriptor} Result.init (Result, callChain)
				end -- if
			end -- inspect
		end -- if
	end -- parseTupleExpression

	parseAssignmentOrQualifiedCall: StatementDescriptor is
	-- Anonymous routine: turple assignment statement (a, b) := expr or ().foo ...
	require
		valid_token: validToken (<<scanner.left_paranthesis_token>>)
	local
		expressions: Array [ExpressionDescriptor]
		exprDsc: ExpressionDescriptor
		callChain: Array [CallChainElement]
		--tuple: TupleExpressionDescriptor
		wtDsc: WritableTupleDescriptor
		toLeave: Boolean
		wasError: Boolean
	do
		scanner.nextToken
		from
			create expressions.make (1, 0)
		until
			toLeave
		loop
			exprDsc := parseExpression
			if exprDsc = Void then
				toLeave := True
			else
				expressions.force (exprDsc, expressions.count + 1)
				inspect
					scanner.token
				when scanner.comma_token then
					scanner.nextToken
				when scanner.right_paranthesis_token then
					scanner.nextToken
					toLeave := True
				else
					syntax_error (<<scanner.comma_token, scanner.right_paranthesis_token>>)
					toLeave := True
					wasError := True
				end -- inpsect
			end -- if
		end -- loop
		if not wasError then
			inspect
				scanner.token
			when scanner.dot_token then
				-- “(”Expression {“,”Expression}“)” “.”Identifier [ Arguments ] {CallChain}
				--                                   ^
				callChain := parseCallChain
				if callChain = Void or else callChain.count = 0 then
					validity_error( "Parenthed expression without a call chain is not a valid statement")
				else
					create wtDsc.init (expressions, callChain)
					if scanner.token = scanner.assignment_token then
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							create {AssignmentStatementDescriptor} Result.init (wtDsc, exprDsc)
						end --if
					else
						Result := wtDsc
					end -- if
				end -- if
			when scanner.assignment_token then
				-- “(”Expression {“,”Expression}“)” “:=” Expression
				--                                  ^
				scanner.nextToken
				exprDsc := parseExpression
				if exprDsc /= Void then
					create wtDsc.init (expressions, Void)
					--create tuple.init (expressions)
					create {AssignmentStatementDescriptor} Result.init (wtDsc, exprDsc)
				end --if
			else
				syntax_error (<<scanner.dot_token, scanner.assignment_token>>)
			end -- inspect
		end -- if
	end -- parseAssignmentOrQualifiedCall
	
	parseLocalAttributesDeclaration (isVarOrRigid: Boolean; name: String): Sorted_Array [LocalAttrDeclarationDescriptor] is
	-- Anonymous rotuine - local attribute declaration
	-- 1: var ...
	-- 2: ident, ....
	-- 3: ident: ....
	-- 4: is Expr 
	require
		valid_token_1: isVarOrRigid implies validToken (<<scanner.var_token, scanner.rigid_token>>)
		valid_token_2: not isVarOrRigid implies name /= Void and then (validToken (<<scanner.comma_token, scanner.colon_token, scanner.is_token>>))	
	local
		type: TypeDescriptor
		detDsc: DetachableTypeDescriptor
		attDsc: AttachedTypeDescriptor
		expr: ExpressionDescriptor
		localDsc: LocalAttrDeclarationDescriptor
		tmpDsc: TemporaryLocalAttributeDescriptor
		isVar, isRigid: Boolean
		localAttrs: Sorted_Array [TemporaryLocalAttributeDescriptor]
		i, n: Integer
		commaFound: Boolean
		toLeave: Boolean
	do
		if isVarOrRigid then
			-- var|rigid ...
			isVar := scanner.token = scanner.var_token
			isRigid := scanner.token = scanner.rigid_token			
			scanner.nextToken
			if scanner.token = scanner.identifier_token then
--trace ("var|rigid processed")
				create tmpDsc.init (isVar, isRigid, scanner.tokenString)
				scanner.nextToken
			else
				syntax_error (<<scanner.identifier_token>>)
			end -- if
		else
			inspect
				scanner.token
			when scanner.comma_token then
				create tmpDsc.init (False, False, name)
				scanner.nextToken
			when scanner.colon_token then
				scanner.nextToken
				type := parseTypeDescriptor
				if type /= Void then
					detDsc ?= type
					if detDsc = Void then
						if scanner.token = scanner.is_token then
							-- initialization
							scanner.nextToken
							expr := parseExpressionWithSemicolon -- parseExpression
							if expr /= Void then
								attDsc ?= type
								check
									valid_attached_type: attDsc /= Void
								end -- check
								create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, name, attDsc, expr)
								create Result.fill (<<localDsc>>)
							end -- if
						else
							attDsc ?= type
							check
								valid_attached_type: attDsc /= Void
							end -- check
							create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, name, attDsc, Void)
							create Result.fill (<<localDsc>>)
							--syntax_error (<<scanner.is_token>>)
						end -- if
					else
						create {DetachedLocalAttributeDeclarationDescriptor} localDsc.init (name, detDsc) --.type)
						create Result.fill (<<localDsc>>)
					end -- if
				end -- if
			when scanner.is_token then
				-- initialization
				scanner.nextToken
				expr := parseExpressionWithSemicolon -- parseExpression
				if expr /= Void then
					create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, name, Void, expr)
					create Result.fill (<<localDsc>>)
				end -- if
			else
				syntax_error (<<scanner.is_token, scanner.comma_token, scanner.colon_token>>)
			end -- inspect	
		end -- if
		if Result = Void and then tmpDsc /= Void then
			-- continue parsing ...
			-- parse local attrs: a, b, c [: Type] is Expr ....
			--                       ^
			from
				create localAttrs.fill (<<tmpDsc>>)
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.var_token, scanner.rigid_token then
					isVar := scanner.token = scanner.var_token
					isRigid := scanner.token = scanner.rigid_token							
					if commaFound or else localAttrs.count = 1 then
						commaFound := False
						scanner.nextToken
						if scanner.token = scanner.identifier_token then
							create tmpDsc.init (isVar, isRigid, scanner.tokenString)
							if not localAttrs.added (tmpDsc) then
								validity_error( "Duplicated local declaration `" + tmpDsc.name + "`") 
							end -- if
							scanner.nextToken
						else
							syntax_error (<<scanner.identifier_token>>)
							toLeave := True
						end -- if
					else
						syntax_error (<<scanner.comma_token, scanner.colon_token>>)
						toLeave := True
					end -- if
				when scanner.identifier_token then
					if commaFound or else localAttrs.count = 1 then
						commaFound := False
						create tmpDsc.init (False, False, scanner.tokenString)
						if not localAttrs.added (tmpDsc) then
							validity_error( "Duplicated local declaration `" + tmpDsc.name + "`") 
						end -- if
						scanner.nextToken
					else
						syntax_error (<<scanner.comma_token, scanner.colon_token>>)
						toLeave := True
					end -- if
				when scanner.comma_token then
					if commaFound then
						syntax_error (<<scanner.identifier_token, scanner.var_token>>)
						toLeave := True
					else
						commaFound := True
						scanner.nextToken
					end -- if
				when scanner.is_token then
					toLeave := True
					scanner.nextToken
--trace ("is expression")
					expr := parseExpressionWithSemicolon -- parseExpression
					if expr /= Void then
						create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, "", Void, expr)
						-- 	init (mVar, mRigid: Boolean; aName: String; aType: like type; ie: like expr)
						if localAttrs /= Void then
							from
								create Result.make
								i := 1
								n := localAttrs.count
							until
								i > n
							loop
								tmpDsc := localAttrs.item (i)
								localDsc := clone (localDsc)
								localdsc.setName (tmpDsc)
								Result.add (localDsc)
								i := i + 1
							end -- if
						end -- if						
					end -- if
				when scanner.colon_token then
					toLeave := True
					scanner.nextToken
					type := parseTypeDescriptor
					if type /= Void then
						detDsc ?= type
						if detDsc = Void then
							if scanner.token = scanner.is_token then
								-- initialization
								scanner.nextToken
								expr := parseExpression
								if expr /= Void then
									attDsc ?= type
									check
										valid_attached_type: attDsc /= Void
									end -- check
									create {AttachedLocalAttributeDeclarationDescriptor} localDsc.init (False, False, "", attDsc, expr)
								end -- if
							else
								syntax_error (<<scanner.is_token>>)
							end -- if
						else
							create {DetachedLocalAttributeDeclarationDescriptor} localDsc.init ("", detDsc) --.type)
						end -- if
						if localDsc /= Void and then localAttrs /= Void then
							from
								create Result.make
								i := 1
								n := localAttrs.count
							until
								i > n
							loop
								tmpDsc := localAttrs.item (i)
								localDsc := clone (localDsc)
								localdsc.setName (tmpDsc)
								Result.add (localDsc)
								i := i + 1
							end -- if
						end -- if
					end -- if					
				else
					if commaFound then
						syntax_error (<<scanner.identifier_token, scanner.var_token, scanner.rigid_token>>)
					else
						syntax_error (<<scanner.comma_token, scanner.colon_token, scanner.is_token>>)
					end -- if
					toLeave := True
				end -- inspect
			end -- loop
		end -- if
	end -- parseLocalAttributesDeclaration
	
	parseAssignmentToIdentifierStatement (name:String): AssignmentStatementDescriptor is
	-- Anonymous routine assignemnt : ident := 
	require
		valid_token: validToken (<<scanner.assignment_token>>)
	local
		writable: IdentifierDescriptor
		expr: ExpressionDescriptor
	do
		create writable.init (name)
		scanner.nextToken
		expr := parseExpression
		if expr /= Void then
			create Result.init (writable, expr)
		end -- if
	end -- parseAssignmentToIdentifierStatement
	
	parseHyperBlock (startingToken: Integer): HyperBlockDescriptor is
	do
		Result ?= parseBlock1 (startingToken, False)
	end -- parseHyperBlock
	parseBlock (startingToken: Integer): StatementDescriptor is
	do
		Result := parseBlock1 (startingToken, True)
	end -- parseBlock

	parseBlock1 (startingToken: Integer; checkForLoop: Boolean): StatementDescriptor is
	--49 HyperBlock : [RequireBlock] InnerBlock [EnsureBlock] end
	--  [RequireBlock] InnerBlock "while" BooleanExpression [EnsureBlock] end
	require
		valid_start_token: startingToken = scanner.require_token or else startingToken = scanner.do_token or else
			scanner.token = scanner.left_curly_bracket_token
	local
		preconditions: Array [PredicateDescriptor]
		postconditions: Array [PredicateDescriptor]
		innerBlock : InnerBlockDescriptor
		loopDsc: LoopStatementDescriptor
	do
		if startingToken = scanner.require_token then
			scanner.nextToken
			-- parse preconditions
			preconditions := parsePredicates
		end -- if
		if scanner.blockStart then
			innerBlock := parseInnerBlock (checkForLoop)
			if innerBlock /= Void then			
				if scanner.token = scanner.ensure_token then
					scanner.nextToken
					-- parse postconditions
					postconditions := parsePredicates
				end -- if		
				if scanner.blockEnd then
					scanner.nextToken
				elseif scanner.Cmode then
					syntax_error (<<scanner.right_curly_bracket_token>>)
				else
					syntax_error (<<scanner.end_routine_expected>>)
				end -- if
				loopDsc ?= innerBlock
				if loopDsc = Void then
					create {HyperBlockDescriptor} Result.init (preconditions, innerBlock.invariantOffList, innerBlock.statements,
						innerBlock.whenClauses, innerBlock.whenElseClause, postconditions)
					-- 	init (rc: like requireClause; lbl: String; invOff: like invariantOffList; stmts: like statements; wc: like whenClauses; wec: like whenElseClause; ec: like ensureClause) is
				else
					loopDsc.setAssertions (preconditions, postconditions)
					Result := loopDsc
				end -- if
			end -- if
		elseif scanner.Cmode then
			syntax_error (<<scanner.left_curly_bracket_token>>)
		else
			syntax_error (<<scanner.do_token>>)
		end -- if
	end -- parseBlock1
	
	parseInnerBlock (checkForLoop: Boolean): InnerBlockDescriptor is 
	-- do [“{”Identifier {“,” Identifier} “}”]  StatementsList [ WhenClause {WhenClause} [else [StatementsList]] ]
	require
		valid_token: validToken (<<scanner.do_token, scanner.left_curly_bracket_token>>)
	local
		invariantOffList: Sorted_Array [String]
		commaFound: Boolean
		wasError: Boolean
		toLeave: Boolean
		listStart: Boolean
		statements: Array [StatementDescriptor]
		whenClauses: Array [WhenClauseDescriptor]
		whenElseClause: Array [StatementDescriptor]
		rwDsc: RepeatWhileDescriptor
	do
--		scanner.disableSemicolon -- No idea why it should be turned off, but it does not work well without this fix
		scanner.nextToken
		if scanner.visibilityStart then
			from
				scanner.nextToken
				listStart := True
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.identifier_token then
					if listStart then
						listStart := False
						create invariantOffList.fill (<<scanner.tokenString>>)
						scanner.nextToken
					elseif commaFound then
						commaFound := False
						if not invariantOffList.added (scanner.tokenString) then
							validity_warning ( "Duplicated entity name `" + scanner.tokenString + "` in file `" + scanner.sourceFileName + "`")
						end -- if
						scanner.nextToken
					else
						if scanner.Cmode then
							syntax_error (<<scanner.identifier_token, scanner.right_square_bracket_token>>)
						else
							syntax_error (<<scanner.identifier_token, scanner.right_curly_bracket_token>>)
						end -- if
						toLeave := True
						wasError := True
					end -- if
				when scanner.comma_token then
					if commaFound then
						if scanner.Cmode then
							syntax_error (<<scanner.identifier_token, scanner.right_square_bracket_token>>)
						else
							syntax_error (<<scanner.identifier_token, scanner.right_curly_bracket_token>>)
						end -- if
						toLeave := True
						wasError := True
					else
						commaFound := True
						scanner.nextToken
					end -- if
				else
					if scanner.visibilityEnd then
						if commaFound then
							syntax_error (<<scanner.identifier_token>>)
							wasError := True
						end -- if
						scanner.nextToken
						toLeave := True
					else
						if commaFound then
							syntax_error (<<scanner.identifier_token>>)
						elseif scanner.Cmode then
							syntax_error (<<scanner.comma_token, scanner.right_square_bracket_token>>)
						else
							syntax_error (<<scanner.comma_token, scanner.right_curly_bracket_token>>)
						end -- if
						toLeave := True
						wasError := True
					end -- if
				end -- loop
			end -- loop
		end -- if
		if not wasError then
			-- 1. parse statements
			-- 2. parse when clauses, then check for else clause
			if checkForLoop then
				statements := parseStatementsWithRepeatWhileCheck (False)
				if statements /= Void then
					rwDsc ?= statements.item (statements.count)
					if rwDsc /= Void then
						statements.resize (1, statements.count - 1) -- get rid of last element
						create {LoopStatementDescriptor} Result.init (invariantOffList, False, rwDsc.whileExpr, Void, statements, Void, Void, Void)
						-- Require and ensure are to be set at upper level !!!
					end -- if
				end -- if
			else
				statements := parseStatements (False)
			end -- if
			if rwDsc = Void then
				if scanner.token = scanner.when_token then
					create whenClauses.make (1, 0)
					create whenElseClause.make (1, 0)
					parseExceptionHandlingCaluse (whenClauses, whenElseClause)
				end -- if
				create Result.init (invariantOffList, statements, whenClauses, whenElseClause)	
			end -- if
		end -- if
	end -- parseInnerBlock

	parseWhenClause: WhenClauseDescriptor is
	-- when [Identifier:] UnitType do StatementsList
	--      ^
	local
		identifier: String
		nmdDsc: NamedTypeDescriptor
		unitType: UnitTypeCommonDescriptor
		wasError: Boolean
	do
		inspect
			scanner.token
		when scanner.identifier_token then
			identifier := scanner.tokenString
			scanner.nextToken
			if scanner.token = scanner.colon_token then
				scanner.nextToken
				nmdDsc := parseUnitType
				if nmdDsc = Void then
					wasError := True
				else
					unitType ?= nmdDsc
					if unitType = Void then
						wasError := True
						validity_error ("Formal generic parameter `" + nmdDsc.name + "` canot be used in when clause")
					end -- if
				end -- if
			else
				wasError := True
				syntax_error (<<scanner.colon_token>>)
			end -- if
		when scanner.type_name_token then
			identifier := scanner.tokenString
			scanner.nextToken
			nmdDsc := parseUnitTypeName1 (identifier, False)
			if nmdDsc = Void then
				wasError := True
			else
				identifier := Void
				unitType ?= nmdDsc
				if unitType = Void then
					wasError := True
					validity_error ("Formal generic parameter `" + nmdDsc.name + "` canot be used in when clause")
				end -- if				
			end -- if
		else
			syntax_error (<<scanner.identifier_token, scanner.type_name_token>>)
			wasError := True
		end -- if
		if not wasError then
			if scanner.blockStart then
				scanner.nextToken
				create Result.init (identifier, unitType, parseStatements (False))
			else
				if scanner.Cmode then
					syntax_error (<<scanner.left_curly_bracket_token>>)
				else
					syntax_error (<<scanner.do_token>>)
				end -- if
				wasError := True
			end -- if		
		end -- if
	end -- parseWhenClause
	
	parseExceptionHandlingCaluse (whenClauses: Array [WhenClauseDescriptor]; whenElseClause: Array [StatementDescriptor]) is
	-- [ WhenClause {WhenClause} [else [StatementsList]]]
	require
		valid_token: validToken (<<scanner.when_token>>)
		non_void_whenClauses: whenClauses /= Void
		non_void_whenElseClause: whenElseClause /= Void
	local
		statements: like whenElseClause
		whenDsc: WhenClauseDescriptor
		toLeave: Boolean
	do
		from
		until
			toLeave
		loop
			if scanner.token = scanner.when_token then
				scanner.nextToken
				whenDsc := parseWhenClause
				if whenDsc = Void then
					toLeave := True
				else
					whenClauses.force (whenDsc, whenClauses.count + 1)
				end -- if
			else
				toLeave := True				
			end -- if
		end -- loop
		if scanner.token = scanner.else_token then
			scanner.nextToken
			statements := parseStatements (False)
			if statements /= Void then
				whenElseClause.append (statements)
			end -- if
		end -- if
	end -- parseExceptionHandlingCaluse
	
	parseStatementsWithRepeatWhileCheck (requireNonEmptyResult: Boolean): Array [StatementDescriptor] is
	do
		Result := parseStatements1 (requireNonEmptyResult, True)
	end -- parseStatementsWithRepeatWhileCheck

	parseStatements (requireNonEmptyResult: Boolean): Array [StatementDescriptor] is
	do
		Result := parseStatements1 (requireNonEmptyResult, False)
	end -- parseStatements
	
	parseStatements1 (requireNonEmptyResult: Boolean; checkForRepeatWhile: Boolean): Array [StatementDescriptor] is
	local
		statements: Array [StatementDescriptor]
		toLeave: Boolean
		statementExpected: Boolean
	do
		from
			create Result.make (1, 0)
			statementExpected := requireNonEmptyResult
		until
			toLeave
		loop
			statements := parseStatement1 (statementExpected, checkForRepeatWhile)
			if statements = Void then
				toLeave := True
			else
				Result.append (statements)
				statementExpected := False
			end -- if
		end -- loop
		if Result.count = 0 then
			Result := Void
		--else
		--	if scanner.token = scanner.colon_token then
		--		-- If last statment was just identifier or type then it is the alternative tag 
		--	end -- if
		end -- if
	end -- parseStatements1

	parseMemberCallWithConstant (constDsc: ConstantDescriptor): ConstantCallDescriptor is
	require
		valid_token: validToken (<<scanner.dot_token>>)
		const_dsc_not_void:  constDsc /= Void
	local
		callChain: Array [CallChainElement]
		arguments: Array [ExpressionDescriptor]
		name1: String
	do
		-- ConstantCall: Constant “.”Identifier [Arguments]  {CallChain}
		--                         ^
		scanner.nextToken
		inspect
			scanner.token
		when scanner.identifier_token, scanner.operator_token,
			scanner.implies_token, scanner.less_token, scanner.greater_token
		then
			-- x.** (expr) is allowed :-)
			name1 := scanner.tokenString
			scanner.nextToken
			arguments := parseArguments
			callChain := parseCallChain
			create Result.init (constDsc, name1, arguments, callChain)
		else
			syntax_error (<<
				scanner.identifier_token, scanner.operator_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token
			>>)
		end -- if
	end -- parseMemberCallWithConstant

	--parseWritableCall(ceDsc: CallDescriptor): CallDescriptor is
	parseWritableCall(ceDsc: ExpressionDescriptor): CallDescriptor is
	-- WritableCall: (((Identifier|return|this) [“.”(Identifier|OperatorName)])|old [“{”UnitTypeName”}”]) [Arguments] {CallChain}
	--#1 Identifier|this|return [“.”Identifier|OperatorName] [Arguments]  {CallChain}
	--                            ^                           ^
	--#2 old [“{”UnitTypeName”}”] [Arguments]  {CallChain}
	--         ^                   ^            ^
	require
		non_void_entity_descriptor: ceDsc /= Void
		valid_token: validToken (<<scanner.dot_token, scanner.left_paranthesis_token, scanner.left_curly_bracket_token>>)
	local
		callChain: Array [CallChainElement]
		arguments: Array [ExpressionDescriptor]
		nmdDsc: NamedTypeDescriptor
		unitType: UnitTypeCommonDescriptor
		name1: String
		constDsc: ConstantDescriptor
		wasError: Boolean
	do
		if ceDsc = oldDsc then
			-- Precursor call
			--#2 old [“{”UnitTypeName”}”] [Arguments]  {CallChain}
			--         ^                   ^            ^
			if scanner.token = scanner.left_curly_bracket_token then
				scanner.nextToken
				nmdDsc := parseUnitType
				if nmdDsc = Void then
					wasError := True
				else
					unitType ?= nmdDsc
					if unitType = Void then
						validity_error ("Formal genric parameter `" + nmdDsc.name + "` can not be used as a parent unit name")
						wasError := True
					end -- if
				end -- if
				if scanner.token = scanner.right_curly_bracket_token then
					scanner.nextToken
				else
					syntax_error (<<scanner.right_curly_bracket_token>>)
					wasError := True
				end -- if
			end -- if
			if not wasError then
				arguments := parseArguments
				callChain := parseCallChain
				create {PrecursorCallDescriptor}Result.init (unitType, arguments, callChain)				
			end -- if
		else
			inspect
				scanner.token
			when scanner.dot_token then
				--#1 Identifier|this|return “.” (Identifier|OperatorName [Arguments]  {CallChain}) | const
				--                           ^
				scanner.nextToken
				inspect
					scanner.token
				when scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token 
				then -- , scanner.bar_token, scanner.tilda_token ????
					-- x.** (expr) is allowed :-)
					name1 := scanner.tokenString
					scanner.nextToken
					arguments := parseArguments
					callChain := parseCallChain
					create {QualifiedCallDescriptor}Result.init (ceDsc, name1, arguments, callChain)
				when scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token then
					constDsc := parseConstant (False)
					check
						npon_void_const_dsc: constDsc /= Void
					end					
					create {QualifiedConstantDescriptor}Result.init (ceDsc, constDsc)
				else
					syntax_error (<<
						scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token,
						scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
					>>)
				end -- if
			when scanner.left_paranthesis_token then
				--#1 Identifier|this|return Arguments {CallChain}
				--                          ^
				arguments := parseArguments
				callChain := parseCallChain
				create {UnqualifiedCallDescriptor}Result.init (ceDsc, arguments, callChain)
			end -- inspect
		end -- if
	end -- parseWritableCall
	
	parseStatement (statementExpected: Boolean): Array [StatementDescriptor] is
	do
		Result := parseStatement1 (statementExpected, False)
	end -- parseStatement

	parseStatement1 (statementExpected, checkForRepeatWhile: Boolean): Array [StatementDescriptor] is
	-- Statement: Assignment| LocalAttributeCreation | ProcedureCall | NewStatement | IfCase | Loop | ? Identifier | Return | Raise | HyperBlock 
	--            ident       var ident                ident (         new            if       while  ?              return   raise   require do
	--                                                 old this 
	--                                                 return
	--                                                 type_name
	local
		name: String
		identDsc: IdentifierDescriptor
		genIdentDsc: GenericIdentifierDescriptor
		factualGenerics: Array [TypeOrExpressionDescriptor]
		nmdDsc: NamedTypeDescriptor
		unitTypeDsc: UnitTypeNameDescriptor
		stmtDsc: StatementDescriptor
		exprDsc: ExpressionDescriptor
		callDsc: CallDescriptor
		assignDsc: AssignmentStatementDescriptor
		initCallDsc: InitCallDescriptor
	do
--trace (">>>parseStatement1")
		inspect
			scanner.token
		when scanner.identifier_token then
			-- Procedure call or assignemnt
			name := scanner.tokenString
			scanner.nextToken
			inspect	
				scanner.token
			when scanner.dot_token, scanner.left_paranthesis_token then
				-- parse call statement: ident. | ident(  .... [:= Expr]
				-- call or assignment!!!
				-- ident. | ident( 
				create identDsc.init (name)
				callDsc := parseWritableCall (identDsc)
				if callDsc /= Void then
					if scanner.token = scanner.assignment_token then
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							-- callDsc := exprDsc1
							if callDsc.isWritable then
								create assignDsc.init (callDsc, exprDsc)
								Result := <<assignDsc>>							
							else
								validity_error( "Left part of assignment is not writable - " + callDsc.out + " := ...")
							end -- if
						end -- if
					else
						-- Statement is the procedure call
						Result := <<callDsc>>
					end -- if
				end -- if 
			when scanner.assignment_token then
				-- assignemnt : ident := 
				stmtDsc := parseAssignmentToIdentifierStatement (name)
				if stmtDsc /= Void then
					Result := <<stmtDsc>>
				end -- if
			when scanner.comma_token, scanner.colon_token, scanner.is_token then
				create Result.make (1, 0)
				parseLocalsDeclaration(Result, False, name)
				if Result.count = 0 then
					Result := Void
				end -- if
			else
				if scanner.genericsStart then
					-- parse for more identifier[...].CallChain
					factualGenerics := parseFactualGenerics
					if factualGenerics /= Void then
						create genIdentDsc.init (name, factualGenerics)				
						inspect	
							scanner.token
						when scanner.dot_token, scanner.left_paranthesis_token then
							-- parse call statement: ident. | ident(  .... [:= Expr]
							-- call or assignment!!!
							-- ident. | ident( 
							callDsc := parseWritableCall (genIdentDsc)
							if callDsc /= Void then
								if scanner.token = scanner.assignment_token then
									scanner.nextToken
									exprDsc := parseExpression
									if exprDsc /= Void then
										-- callDsc := exprDsc1
										if callDsc.isWritable then
											create assignDsc.init (callDsc, exprDsc)
											Result := <<assignDsc>>							
										else
											validity_error( "Left part of assignment is not writable - " + callDsc.out + " := ...")
										end -- if
									end -- if
								else
									-- Statement is the procedure call
									Result := <<callDsc>>
								end -- if
							end -- if 
						else
							syntax_error (<< scanner.left_paranthesis_token, scanner.dot_token>>)
						end -- inspect
					end -- if
				else
					-- That is just a procedure call with no arguments!!! 
					create identDsc.init (name)
					create {UnqualifiedCallDescriptor} stmtDsc.init (identDsc, Void, Void)
					Result := <<stmtDsc>>
				end -- if
			end
		when scanner.type_name_token then
			-- Module call or asignment into module attribute  - NOT SUPPORTED !!!!
			name := scanner.tokenString
			scanner.nextToken
			if scanner.genericsStart then
				-- parse for Type [ ] 
				nmdDsc := parseUnitTypeName1 (name, False)
				if nmdDsc /= Void then
					unitTypeDsc ?= nmdDsc
				end -- if				
				--unitTypeDsc := parseUnitTypeName1 (name, False)
			else
				create unitTypeDsc.init (name, Void)
				unitTypeDsc ?= register_named_type (unitTypeDsc)
				if unitTypeDsc = Void then
					validity_error ("Formal generic parameter `" + name + "` cannot be used as a module")
				end -- if
			end -- if 
			if unitTypeDsc /= Void then
				inspect
					scanner.token
				when scanner.left_paranthesis_token then
					-- parse init call: UnitName(  .... 
					--create unitTypeDsc.init (name, Void)
					create {InitCallDescriptor}initCallDsc.init (unitTypeDsc, parseArguments)
					if currentUnitDsc.name.is_equal (name) then
						Result := <<initCallDsc>>
					else
						validity_error ("Initializer for unit `" + name + "` can not be called within the unit `" + currentUnitDsc.name + "`")
					end -- if
				when scanner.dot_token then
					-- ModuleName. 
					--create unitTypeDsc.init (name, Void)				
					stmtDsc := parseCallWithOptionalTailAssignment (unitTypeDsc)
					if stmtDsc /= Void then
						Result := <<stmtDsc>>
					end -- if
				else
					syntax_error (<< scanner.left_paranthesis_token, scanner.dot_token>>)
				end -- inspect
			end -- if
		when scanner.var_token, scanner.rigid_token then
			create Result.make (1, 0)
			parseLocalsDeclaration(Result, True, Void)
			if Result.count = 0 then
				Result := Void
			end -- if
		when scanner.left_paranthesis_token then
			stmtDsc := parseAssignmentOrQualifiedCall
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.old_token then
			scanner.nextToken
			inspect
				scanner.token 
			when scanner.left_paranthesis_token, scanner.left_curly_bracket_token then
				stmtDsc := parseWritableCall (oldDsc)
				if stmtDsc /= Void then
					Result := <<stmtDsc>>
				end -- if
			else
				Result := <<oldDsc>> -- Precursor call
			end -- inspect
		when scanner.this_token then
			scanner.nextToken
			inspect
				scanner.token 
			when scanner.dot_token, scanner.left_paranthesis_token then
				callDsc := parseWritableCall (thisDsc)
				if callDsc /= Void then
					if scanner.token = scanner.assignment_token then
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							-- callDsc := exprDsc1
							if callDsc.isWritable then
								create assignDsc.init (callDsc, exprDsc)
								Result := <<assignDsc>>							
							else
								validity_error( "Left part of assignment is not writable - " + callDsc.out + " := ...")
							end -- if
						end -- if
					else
						-- Expression is the this.|( call statement!!!
						Result := <<callDsc>>
					end -- if
				end -- if
			else
				syntax_error (<<scanner.dot_token, scanner.left_paranthesis_token>>)
			end -- inspect
		when scanner.new_token then
			stmtDsc := parseNewStatement
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.if_token then
			stmtDsc := parseIfStatement
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.while_token then
			stmtDsc := parseWhileStatement (checkForRepeatWhile)
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.detach_token then
			stmtDsc := parseDetachStatement
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.return_token then
			stmtDsc := parseReturnStatement
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.raise_token then
			stmtDsc := parseRaiseStatement
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		when scanner.require_token then
			stmtDsc := parseBlock (scanner.token)
			if stmtDsc /= Void then
				Result := <<stmtDsc>>
			end -- if
		else
			if scanner.blockStart then
				stmtDsc := parseBlock (scanner.token)
				if stmtDsc /= Void then
					Result := <<stmtDsc>>
				end -- if
			elseif scanner.Cmode then
				if statementExpected then
					syntax_error (<<
						scanner.identifier_token, scanner.var_token, scanner.left_paranthesis_token, scanner.old_token, scanner.this_token, scanner.new_token,
						scanner.if_token, scanner.while_token, scanner.detach_token, scanner.return_token, scanner.raise_token, scanner.require_token, 
						scanner.left_curly_bracket_token, scanner.operator_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token
					>>)
				end -- if
			else
				if statementExpected then
					syntax_error (<<
						scanner.identifier_token, scanner.var_token, scanner.left_paranthesis_token, scanner.old_token, scanner.this_token, scanner.new_token,
						scanner.if_token, scanner.while_token, scanner.detach_token, scanner.return_token, scanner.raise_token, scanner.require_token, 
						scanner.do_token, scanner.operator_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token
					>>)
				end -- if
			end --if
			-- it is not a statement
		end -- inspect
--trace ("<<<parseStatement1")
	end -- parseStatement1
	
	parseExpressionWithSemicolon: ExpressionDescriptor is
	do
--trace (">>>parseExpressionWithSemicolon")
		Result := parseExpression1 (False, True, True, False)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
--trace ("<<<; removed parseExpressionWithSemicolon")
		else
--trace ("<<<parseExpressionWithSemicolon")
		end -- if
	end -- parseExpressionWithSemicolon
	parseExpressionWithSemicolon1 (checkSemicolonAfter: Boolean): ExpressionDescriptor is
	do
		Result := parseExpression1 (False, True, checkSemicolonAfter, False)
	end -- parseExpressionWithSemicolon1

	parseExpressionX: ExpressionDescriptor is
	do
		Result := parseExpression1 (False, True, False, True)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
		end -- if
	end -- parseExpressionX
	parseOptionalExpressionX: ExpressionDescriptor is
	do
		Result := parseExpression1 (False, False, False, True)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
		end -- if
	end -- parseOptionalExpressionX
	
	parseExpression: ExpressionDescriptor is
	do
		Result := parseExpression1 (False, True, False, False)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
		end -- if
	end -- parseExpression
	parseOptionalExpression: ExpressionDescriptor is
	do
		Result := parseExpression1 (False, False, False, False)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
		end -- if
	end -- parseOptionalExpression
	parseCommentedExpression: ExpressionDescriptor is
	-- predicates only
	do
		Result := parseExpression1 (True, False, False, False)
		if scanner.token = scanner.semicolon_token then
			scanner.nextToken
		end -- if
	end -- parseCommentedExpression

	thisDsc: ThisDescriptor is
	once
		create Result
	end -- thisDsc
	returnDsc: ReturnDescriptor is
	once
		create Result
	end -- returnDsc
	oldDsc: OldDescriptor is
	once
		create Result
	end -- oldDsc

	parseUnaryExpression (operator: String; checkForCommentAfter, checkSemicolonAfter: Boolean): ExpressionDescriptor is
	require
		operator_not_void: operator /= Void
	local
		exprDsc: ExpressionDescriptor
		constDsc: ConstantDescriptor
		identDsc: IdentifierDescriptor
		--exprCall: ExpressionCallDescriptor
		--exprCall1: ExpressionCallDescriptor
		cceDsc: CallChainElement 
		--cceDsc1: CallChainElement
	do
--trace (">>>parseUnaryExpression")
		inspect
			scanner.token
		when scanner.identifier_token then 
			create identDsc.init (scanner.tokenString)
			scanner.nextWithSemicolon (checkSemicolonAfter)
			create {CallChainElement} cceDsc.init (operator, Void)
			create {ExpressionCallDescriptor} Result.init (identDsc, <<cceDsc>>)
		when scanner.this_token then
			scanner.nextWithSemicolon (checkSemicolonAfter)
			create {CallChainElement} cceDsc.init (operator, Void)
			create {ExpressionCallDescriptor} Result.init (thisDsc, <<cceDsc>>)
		when scanner.return_token then
			scanner.nextWithSemicolon (checkSemicolonAfter)
			create {CallChainElement} cceDsc.init (operator, Void)
			create {ExpressionCallDescriptor} Result.init (returnDsc, <<cceDsc>>)
		when scanner.integer_const_token, scanner.real_const_token then
			if operator.is_equal ("+") then
				-- ignore plus sign
				Result := parseConstant (checkSemicolonAfter)				
			elseif operator.is_equal ("-") then
				-- negate the constant 
				constDsc := parseConstant (checkSemicolonAfter)
				constDsc.negate
				Result := constDsc
			else				
				constDsc := parseConstant (checkSemicolonAfter)
				check
					non_void_constant_dsc: constDsc /= Void
				end
				create {CallChainElement} cceDsc.init (operator, Void)
				create {ExpressionCallDescriptor} Result.init (constDsc, <<cceDsc>>)
			end -- if
		when scanner.string_const_token, scanner.char_const_token then
			constDsc := parseConstant (checkSemicolonAfter)
			check
				non_void_constant_dsc: constDsc /= Void
			end
			create {CallChainElement} cceDsc.init (operator, Void)
			create {ExpressionCallDescriptor} Result.init (constDsc, <<cceDsc>>)
		when scanner.old_token then
			-- operator old ....
			-- OldExpression
			scanner.nextToken
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
				create {OldExpressionDescriptor} Result.init (exprDsc)
				create {CallChainElement} cceDsc.init (operator, Void)
				create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
			end -- if
		when scanner.left_paranthesis_token then
			scanner.nextToken
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
				if scanner.token = scanner.right_paranthesis_token then
					scanner.nextWithSemicolon (checkSemicolonAfter)
					-- (exprDsc).operator ()
					create {CallChainElement} cceDsc.init (operator, Void)
					create {ExpressionCallDescriptor} Result.init (exprDsc, <<cceDsc>>)
					--create {OperatorExpressionDescriptor} Result.init (operator, exprDsc)
				else
					syntax_error (<<scanner.right_paranthesis_token>>)
				end -- if
			end -- if
		else
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
				scanner.nextWithSemicolon (checkSemicolonAfter)
				-- (exprDsc).operator ()
				create {CallChainElement} cceDsc.init (operator, Void)
				create {ExpressionCallDescriptor} Result.init (exprDsc, <<cceDsc>>)
				--create {OperatorExpressionDescriptor} Result.init (operator, exprDsc)
			end -- if
		end -- inspect
	end -- parseUnaryExpression

	parseExpression1 (checkForCommentAfter, isMandatory, checkSemicolonAfter, returnType: Boolean): ExpressionDescriptor is
	--50 Expression:
	-- 		IfExpression | MemberCall | NewExpression | Expression Operator Expression | Operator Expression | Constant | TypeOfExpression |
	--      if             ( ident ...  new                                              operator              constant
	-- 		OldExpression | RangeExpression | LambdaExpression | TupleExpression | RefExpression | “(”Expression“)”{CallChain}
	--      old                               pure safe rtn      (                 ref               (

	local	
		name: String
		exprDsc: ExpressionDescriptor
		constDsc: ConstantDescriptor
		identDsc: IdentifierDescriptor
		--exprOpExpr: ExprOperatorExprDescriptor
		rangeDsc: ExpressionDescriptor
		cceDsc: CallChainElement 
		operator: String
		toParseMore: Boolean
		toExit: Boolean
		utnDsc: NamedTypeDescriptor -- UnitTypeNameDescriptor
		exprCall: ExpressionCallDescriptor
		exprCall1: ExpressionCallDescriptor
		cceDsc1: CallChainElement
		order: Integer
	do
debug
	--trace (">>> parseExpression1") -- with checkSemicolonAfter: + checkSemicolonAfter.out)
end
		inspect
			scanner.token
		when scanner.if_token then
			-- IfExpression
			Result := parseIfExpression (checkSemicolonAfter)
		when scanner.new_token then
			-- NewExpression
			Result := parseNewExpression
		when scanner.old_token then
			-- OldExpression
			scanner.nextToken
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
				create {OldExpressionDescriptor} Result.init (exprDsc)
			end -- if
		when scanner.rtn_token, scanner.pure_token, scanner.safe_token then
			-- LambdaExpression
			Result := parseLambdaExpression (checkSemicolonAfter)
		when scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token then
			-- operator Expression
			operator := scanner.tokenString
debug
--	trace ("#1:Unary operator " + Result.out)
end -- debug
			scanner.nextToken
			Result := parseUnaryExpression (operator, checkForCommentAfter, checkSemicolonAfter)
--trace ("#1:Unary operator " + Result.out)
		when scanner.left_paranthesis_token then
			-- “(”Expression“)” or tuple “(”Expression {", "Expression}“)” {CallChain}
			scanner.nextToken
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
--trace ("#2: (Expression " + exprDsc.out)
				inspect
					scanner.token
				when scanner.right_paranthesis_token then				
					scanner.nextWithSemicolon (checkSemicolonAfter)
					inspect
						scanner.token
					when scanner.dot_token then
						create {ExpressionCallDescriptor} Result.init (exprDsc, parseCallChain)
					--when scanner.semicolon_token then
					--	--scanner.nextToken Keep semicolon till the caller to remove it
					--	create {ParenthedExpressionDescriptor} Result.init (exprDsc)
					else
						create {ParenthedExpressionDescriptor} Result.init (exprDsc)
					end -- if
--trace ("#4: (Expr) " + Result.out)
				when scanner.comma_token then
					-- It is a turple expression !!!!
					Result := parseTupleExpression (exprDsc)
					if Result /= Void then
						if scanner.token = scanner.dot_token then
							create {ExpressionCallDescriptor} Result.init (Result, parseCallChain)
						end -- if
					end -- if
				else
--trace ("#3parseExpression1")
					syntax_error (<<scanner.right_paranthesis_token>>)
				end -- inspect
			end -- if			
		when scanner.ref_token then
			-- RefExpression
			scanner.nextToken
			exprDsc := parseExpression1 (checkForCommentAfter, True, checkSemicolonAfter, False)
			if exprDsc /= Void then
				create {RefExpressionDescriptor} Result.init (exprDsc)
			end -- if
		when scanner.return_token then
			scanner.nextWithSemicolon (checkSemicolonAfter)
			inspect
				scanner.token
			when scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token then
				-- ident operator
				Result := parseBinaryOperatorExpression (returnDsc, checkSemicolonAfter)
			when scanner.in_token then
				-- return in Expr1 [.. Expr2]
				scanner.nextToken
				rangeDsc := parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter)
				if rangeDsc /= Void then
--trace ("#1<return>: " + returnDsc.out + " in " + rangeDsc.out)
					create {InRangeExpression}Result.init (returnDsc, rangeDsc)
				end -- if
			when scanner.bar_token then 
--				if stopAtBar then
--					Result := returnDsc
--				else
--					Result := parseBinaryOperatorExpression (returnDsc, checkSemicolonAfter)
--				end -- if
				Result := parseBinaryOperatorExpression (returnDsc, checkSemicolonAfter)
			when scanner.dot_token, scanner.left_paranthesis_token then
				-- return. | return( 
				Result := parseWritableCall (returnDsc)
			when scanner.period_token, scanner.left_curly_bracket_token then
				-- return .. Expr kind of range expression
				Result := parseRangeExpression (returnDsc, checkSemicolonAfter)
			when scanner.semicolon_token then
				-- Just return
				--	Keep semicolon!!! scanner.nextToken
				Result := returnDsc
				toExit := True
			else
				-- Just return
				Result := returnDsc
			end -- inspect
		when scanner.type_name_token then
			name := scanner.tokenString
			scanner.nextToken
			utnDsc := parseUnitTypeName1 (name, False)
			if utnDsc = Void then
				toExit := True
			else
				inspect
					scanner.token
				when scanner.dot_token, scanner.left_paranthesis_token then
					Result := parseWritableCall (utnDsc)
				else
					if returnType then
						-- Just type !!!
						create {UnitTypeAlternative} Result.init (utnDsc)						
					else
						-- assume: new Type
						create {NewExpressionDescriptor} Result.init (utnDsc, Void)
					end -- if
				end
			end -- if
		when scanner.identifier_token then
			name := scanner.tokenString
			if name.is_equal ("not") then
				-- operator Expression
				scanner.nextToken
				Result := parseUnaryExpression (name, checkForCommentAfter, checkSemicolonAfter)
--trace ("#2:Unary operator " + operator)
			else
				create identDsc.init (name)
				scanner.nextWithSemicolon (checkSemicolonAfter)
debug
	--trace ("<ident>: " + identDsc.out)
end -- debug
				inspect
					scanner.token
				when scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token then
					-- ident operator
debug
	--trace ("<ident>: " + identDsc.out + " <operator>" )
end -- debug
					Result := parseBinaryOperatorExpression (identDsc, checkSemicolonAfter)
				when scanner.in_token then
					-- ident in Expr1 .. Expr2
--trace ("<ident>: " + identDsc.out + " in <range>" )
					scanner.nextToken
					rangeDsc := parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter)
					if rangeDsc /= Void then
--trace ("#2<ident>: " + identDsc.out + " in " + rangeDsc.out)
						create {InRangeExpression}Result.init (identDsc, rangeDsc)
					end -- if
				when scanner.bar_token then 
--trace ("<ident>: " + identDsc.out + " |" )
--					if stopAtBar then
--						Result := identDsc
--						--create {IdentifierDescriptor} Result.init (name)
--					else
--						Result := parseBinaryOperatorExpression (identDsc, checkSemicolonAfter)
--					end -- if
					Result := parseBinaryOperatorExpression (identDsc, checkSemicolonAfter)
				when scanner.dot_token, scanner.left_paranthesis_token then
					-- ident. | ident( 
--trace ("<ident>: " + identDsc.out + " .|(" )
					Result := parseWritableCall (identDsc)
--trace ("Expr: " + Result.out)
				when scanner.semicolon_token then
					-- Just identiifer
					--	Keep semicolon!!! scanner.nextToken
--trace ("<ident>: " + identDsc.out + ";" )
					Result := identDsc
					toExit := True
				when scanner.period_token then
					-- ident .. Expr kind of range expression
					-- ident {ConstExpr} .. 
--trace ("<ident>: " + identDsc.out + " ..|{" )
					Result := parseRangeExpression (identDsc, checkSemicolonAfter)
				else
					if scanner.Cmode then
						inspect
							scanner.token
						when scanner.left_square_bracket_token then
							-- ident {ConstExpr} .. 
--trace ("<ident>: " + identDsc.out + " ..|{" )
							Result := parseRangeExpression (identDsc, checkSemicolonAfter)
						when scanner.less_token then 
							-- ident [    like a := Bit [5].count
--trace ("<ident>: " + identDsc.out + " [" )
							utnDsc := parseUnitTypeName1 (name, False)
								--	parseUnitTypeName1 (name: String; checkSemicolonAfter: Boolean): UnitTypeNameDescriptor is
							if utnDsc /= Void then
								Result := utnDsc
							end -- if		
						else
--trace ("<ident>: " + identDsc.out)
							-- Just identiifer
							Result := identDsc
						end -- inspect
					else
						inspect
							scanner.token
						when scanner.left_curly_bracket_token then
							-- ident {ConstExpr} .. 
--trace ("<ident>: " + identDsc.out + " ..|{" )
							Result := parseRangeExpression (identDsc, checkSemicolonAfter)
						when scanner.left_square_bracket_token then 
							-- ident [    like a := Bit [5].count
--trace ("<ident>: " + identDsc.out + " [" )
							utnDsc := parseUnitTypeName1 (name, False)
								--	parseUnitTypeName1 (name: String; checkSemicolonAfter: Boolean): UnitTypeNameDescriptor is
							if utnDsc /= Void then
								Result := utnDsc
							end -- if		
						else
debug
	--trace ("<ident>: " + identDsc.out)
end -- debug
							-- Just identiifer
							Result := identDsc
						end -- inspect
					end -- if
				end -- inspect
			end -- if
		when scanner.this_token then
			scanner.nextWithSemicolon (checkSemicolonAfter)
--trace ("this ")
			inspect
				scanner.token
			when scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token then
				-- this operator
				Result := parseBinaryOperatorExpression (thisDsc, checkSemicolonAfter)
			when scanner.bar_token then 
--				if stopAtBar then
--					Result := thisDsc
--				else
--					Result := parseBinaryOperatorExpression (thisDsc, checkSemicolonAfter)
--				end -- if
				Result := parseBinaryOperatorExpression (thisDsc, checkSemicolonAfter)
			when scanner.in_token then
				-- this in Expr1 .. Expr2
				scanner.nextToken
				rangeDsc := parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter)
				if rangeDsc /= Void then
--trace ("#3<this>: " + thisDsc.out + " in " + rangeDsc.out)
					create {InRangeExpression}Result.init (thisDsc, rangeDsc)
				end -- if
			when scanner.dot_token, scanner.left_paranthesis_token then
				-- this. | this( 
--trace ("this.|(")
				Result := parseWritableCall (thisDsc)
--trace ("this.|( -> " + Result.out )				
			when scanner.period_token, scanner.left_curly_bracket_token then
				-- this .. Expr kind of range expression
				Result := parseRangeExpression (thisDsc, checkSemicolonAfter)
			when scanner.semicolon_token then
				-- Just this
				--	Keep semicolon!!! scanner.nextToken
				Result := thisDsc
				toExit := True
			else
				-- Just this
				Result := thisDsc
			end -- inspect
		when scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token then
			constDsc := parseConstant (checkSemicolonAfter)
			check
				non_void_constant_dsc: constDsc /= Void
			end
			inspect
				scanner.token
			when scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token
			then 
				Result := parseBinaryOperatorExpression (constDsc, checkSemicolonAfter)
			when scanner.in_token then
				-- const in Expr1 .. Expr2
				scanner.nextToken
				rangeDsc := parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter)
				if rangeDsc /= Void then
--trace ("#4<const>: " + constDsc.out + " in " + rangeDsc.out)
					create {InRangeExpression}Result.init (constDsc, rangeDsc)
				end -- if
			when scanner.bar_token then 
--				if stopAtBar then
--					Result := constDsc
--				else
--					Result := parseBinaryOperatorExpression (constDsc, checkSemicolonAfter)
--				end -- if
				Result := parseBinaryOperatorExpression (constDsc, checkSemicolonAfter)
			when scanner.period_token, scanner.left_curly_bracket_token then
				-- ident .. Expr kind of range expression
				Result := parseRangeExpression (constDsc, checkSemicolonAfter)
			when scanner.dot_token then
				-- const.callChain Huh ...
				Result := parseMemberCallWithConstant (constDsc) -- ConstantDescriptor
			when scanner.semicolon_token then
				-- Just constant
				--	Keep semicolon!!! scanner.nextToken
				Result := constDsc
				toExit:= True
			else
				-- Just constant
				Result := constDsc
				toExit:= True
			end -- inspect
		else
			if isMandatory then
				syntax_error (<<
					scanner.if_token, scanner.new_token, scanner.old_token, scanner.rtn_token, scanner.pure_token, scanner.safe_token,
					scanner.left_paranthesis_token, scanner.ref_token, scanner.identifier_token, scanner.this_token, scanner.return_token, 
					scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
				>>)				
			end -- if
		end -- inpsect
		if Result /= Void and then not toExit then
			from
debug
	--trace ("#55: parse expression tail")
end -- debug
			until
				toExit
			loop
				inspect
					scanner.token
				when scanner.semicolon_token then -- no need to continue with expression
					--	Keep semicolon!!! scanner.nextToken
--trace ("#55 EOE due to ;")
					toExit := True
				when
					scanner.operator_token, scanner.implies_token, scanner.bar_token, scanner.tilda_token, scanner.less_token, scanner.greater_token
				then
					-- Expr operator Expr
					operator := scanner.tokenString
					scanner.nextToken
					exprDsc := parseExpression1 (checkForCommentAfter, isMandatory, checkSemicolonAfter, False)
					if exprDsc = Void then
						toExit := True
					else
						-- Result.operator (exprDsc)
--trace ("#1: " + Result.out + " " + operator + "[" + getOrder (operator).out + "] " + exprDsc.out + "[" + exprDsc.getOrder.out + "]")
--trace ("#1: " + Result.out + " " + operator + exprDsc.out)
						order := exprDsc.getOrder
						if order = 0 then
							create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
							create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
						elseif getOrder (operator) >= order then
							create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
							create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
						else
							exprCall ?= exprDsc
							check
								non_void_expr_call: exprCall /= Void
								valid_call_chain: exprCall.callChain.count > 0
							end -- check
							create cceDsc1.init (operator, <<exprCall.expression>>)
							create exprCall1.init (Result, <<cceDsc1>>)
							create {ExpressionCallDescriptor} Result.init (exprCall1, <<exprCall.callChain.item (1)>>)
--trace ("#1.1:[" + Result.getOrder.out + "]" + Result.out)
						end -- if
--trace ("#1.2: " + Result.out)
						--if getOrder (operator) > exprDsc.getOrder then
						--	create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
						--	create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
						--else
						--	create {CallChainElement} cceDsc.init (operator, <<Result>>)
						--	create {ExpressionCallDescriptor} Result.init (exprDsc, <<cceDsc>>)
						--end -- if
						----create  {ExprOperatorExprDescriptor} Result.init (Result, operator, exprDsc)
						------Result := exprOpExpr
					end -- if
				when scanner.is_token then
					toExit := True
					--scanner.push
					scanner.nextToken
					inspect 
						scanner.token
					when scanner.detach_token then
						-- expr is ? 
						--		   ^
						--scanner.flush
						scanner.nextToken
						create {IsDetachedDescriptor} Result.init (Result)
					when scanner.type_name_token, scanner.ref_token, scanner.val_token then
						-- expr is Type
						--		   ^
						utnDsc := parseUnitType2 (checkSemicolonAfter)
						if utnDsc /= Void then
							create {IsAttachedDescriptor} Result.init (Result, utnDsc)
						end -- if
						--if utnDsc = Void then
						--	-- Not sure if Void is to be returned in case of error parsing the type
						--	-- Result := Void
						--else
						--	inspect 
						--		scanner.token
						--	when scanner.do_token then
						--		scanner.flush
						--		-- expr is Type do
						--		--		        ^
						--		create {IsAttachedDescriptor} Result.init (Result, utnDsc)
						--	else
						--		-- may be the tag start
						--		-- expr is Type {"," Type} :
						--		scanner.revert
						--	end -- inspect
						--end -- if
					else
						syntax_error (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.detach_token>>)
						-- expr is expr :
						--scanner.revert
					end -- inspect
					
--					if scanner.token = scanner.colon_token then
--						-- That is if is alternative:  start, end of this expression!!!
--						scanner.push
--						scanner.revert
--						debug
--							trace ("`is identifier` found !")
--						end
--						toExit := True
--					else
--						scanner.flush
--						-- That is is expression - type of expression: TypeOfExpression: Expression is (“?”| UnitType)
--						--Result := parseTypeOfExpression (Result, checkSemicolonAfter)
--						inspect
--							scanner.token
--						when scanner.detach_token then
--							-- expr is ?
--							scanner.nextToken
--							create {IsDetachedDescriptor} Result.init (Result)
--						else
--							utnDsc := parseUnitType2 (checkSemicolonAfter)
--							if utnDsc = Void then
--								-- Not sure if Void is to be retruned in case of error parsing the type
--								-- Result := Void
--							else
--								-- expr is Type
--								create {IsAttachedDescriptor} Result.init (Result, utnDsc)
--							end -- if
--						end -- inspect
--						toExit := True
--					end -- if
				when scanner.in_token then 
					-- Expr in Expr1 .. Expr2
					scanner.nextToken
					rangeDsc := parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter)
					if rangeDsc /= Void then
--trace ("#5<expr>: " + Result.out + " in " + rangeDsc.out)
						create {InRangeExpression}Result.init (Result, rangeDsc)
					end -- if
					toExit := True
				when scanner.identifier_token then
					-- expr or else .... for example ....
					operator := scanner.tokenString
					-- ugly hack :-( 'not' not supported at all ...
					if	operator.is_equal ("and") then
						toParseMore := True
						scanner.nextToken
						if scanner.tokenString.is_equal ("then") then
							operator := "and then"
							scanner.nextToken
						end -- if
					elseif operator.is_equal ("or") then
						toParseMore := True
						scanner.nextToken
						if scanner.tokenString.is_equal ("else") then
							operator := "or else"
							scanner.nextToken
						end -- if
					elseif operator.is_equal ("xor") or else operator.is_equal ("implies") then 
						toParseMore := True
						scanner.nextToken
					else
--trace ("Expr: " + Result.out + " `" + scanner.tokenString + "`")
						toExit := True
					end -- if
debug
	--trace ("#66 Expr: " + Result.out + " `" + operator + "`")
end -- debug
					if toParseMore then
						toParseMore := False
						exprDsc := parseExpression1 (checkForCommentAfter, isMandatory, checkSemicolonAfter, False)
						if exprDsc = Void then
							 toExit := True
						else
							-- Result.operator (exprDsc)
debug
	--trace ("#2: " + Result.out + " " + operator + "[" + getOrder (operator).out + "] " + exprDsc.out + "[" + exprDsc.getOrder.out + "]")
end
							order := exprDsc.getOrder
							if order = 0 then
								create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
								create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
							elseif getOrder (operator) >= order then
								create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
								create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
							else
								exprCall ?= exprDsc
								check
									non_void_expr_call: exprCall /= Void
									valid_call_chain: exprCall.callChain.count > 0
								end -- check
								create cceDsc1.init (operator, <<exprCall.expression>>)
								create exprCall1.init (Result, <<cceDsc1>>)
								create {ExpressionCallDescriptor} Result.init (exprCall1, <<exprCall.callChain.item (1)>>)
--trace ("#2.1:[" + Result.getOrder.out + "]" + Result.out)
							end -- if
debug
	--trace ("#2.2: " + Result.out)
end -- debug
							--if getOrder (operator) > exprDsc.getOrder then
							--	create {CallChainElement} cceDsc.init (operator, <<exprDsc>>)
							--	create {ExpressionCallDescriptor} Result.init (Result, <<cceDsc>>)
							--else
							--	create {CallChainElement} cceDsc.init (operator, <<Result>>)
							--	create {ExpressionCallDescriptor} Result.init (exprDsc, <<cceDsc>>)
							--end -- if
							---- create {ExprOperatorExprDescriptor} Result.init (Result, operator, exprDsc)
							------ Result := exprOpExpr
						end -- if
					end -- if
				else
--trace ("EOE")
					-- end of expression
					toExit := True
				end -- inspect
			end -- loop			
		end -- if
debug
	if Result = Void then
		--trace ("<<< parseExpression1: <Void>")
	else
		--trace ("<<< parseExpression1: " + Result.out)
	end -- if
end -- debug
	end -- parseExpression1


	--parseTypeOfExpression (exprDsc: ExpressionDescriptor; checkSemicolonAfter: Boolean): TypeOfExpressionDescriptor is 
	--	-- That is is expression - type of expression: TypeOfExpression: Expression is (“?”| UnitType)
	--require
	--	expr_not_void: exprDsc /= Void
	--local
	--	utdDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
	--do
	--	inspect
	--		scanner.token
	--	when scanner.detach_token then
	--		-- Expr is ?
	--		scanner.nextToken
	--		create {IsDetachedDescriptor} Result.init (exprDsc)
	--	else
	--		utdDsc := parseUnitType2 (checkSemicolonAfter)
	--		if utdDsc /= Void then
	--			create {IsAttachedDescriptor} Result.init (exprDsc, utdDsc)
	--		end -- if
	--	end -- inspect
	--end -- parseTypeOfExpression

	parseCallChainElement: CallChainElement is
	-- Identifier [ Arguments ]
	local
		identifier: String
	do
		inspect 
			scanner.token
		when scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
			scanner.implies_token, scanner.less_token, scanner.greater_token
		then
			identifier := scanner.tokenString
			scanner.nextToken			
			create Result.init (identifier, parseArguments)
		else
			syntax_error (<<
				scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token
			>>)
		end -- inspect
	end -- if

	parseCallChain: Array [CallChainElement] is
	-- {CallChain}
	local
		ccElement: CallChainElement
		toLeave : Boolean
	do	
		from
			create Result.make (1, 0)
		until
			toLeave
		loop
			if scanner.token = scanner.dot_token then
				scanner.nextToken
				ccElement := parseCallChainElement
				if ccElement /= Void then
					Result.force (ccElement, Result.count + 1)
				end -- if
			else
				toLeave := True
			end -- if
		end -- loop
	end -- parseCallChain
	
	parseMandatoryRangeExpression (checkForCommentAfter, checkSemicolonAfter: Boolean): ExpressionDescriptor is
	local
		left: ExpressionDescriptor
	do
		left := parseExpression1 (checkForCommentAfter, True checkSemicolonAfter, False)
		if left /= Void then
			inspect
				scanner.token
			when scanner.period_token, scanner.left_curly_bracket_token then
				Result := parseRangeExpression (left, checkSemicolonAfter)
			else
				Result := left
			end -- inspect
		end -- if
	end -- parseMandatoryRangeExpression

	parseRangeExpression (left: ExpressionDescriptor; checkSemicolonAfter: Boolean): RangeExpressionDescriptor is
	--32 Expression [“{”OperatorName ConstantExpression "}"] “..”Expression
	-- Conflict !!!! a is 5 {} foo do end !!!
	require
		valid_token: validToken (<<scanner.period_token, scanner.left_curly_bracket_token, scanner.left_square_bracket_token>>)
		non_void_lower_expr: left /= Void
	local
		right: ExpressionDescriptor
		operator: String
		exprDsc: ExpressionDescriptor	
		wasError: Boolean
	do
		if scanner.visibilityStart then
			scanner.nextToken
			inspect
				scanner.token
			when scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
			then
				operator := scanner.tokenString
				scanner.nextToken
				exprDsc := parseExpression
				if exprDsc /= Void then
					if scanner.visibilityEnd then
						scanner.nextToken
					elseif scanner.Cmode then
						syntax_error (<<scanner.right_square_bracket_token>>)
						wasError := True
					else
						syntax_error (<<scanner.right_curly_bracket_token>>)
						wasError := True
					end -- if
				end -- if
			else
				syntax_error (<<
					scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
					scanner.implies_token, 
					scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
				>>)
				wasError := True
			end -- if
		end -- if
		if not wasError then
			if scanner.token = scanner.period_token then
				scanner.nextToken
				right := parseExpressionWithSemicolon1 (checkSemicolonAfter)
				if right /= Void then
					create Result.init (left, operator, exprDsc, right)
				end -- if
			else
--trace ("#3")
				syntax_error (<<scanner.period_token>>)
			end -- if
		end -- if
	end -- parseRangeExpression
	
	parseBinaryOperatorExpression (exprDsc1: ExpressionDescriptor; checkSemicolonAfter: Boolean ): ExpressionCallDescriptor is --ExprOperatorExprDescriptor is
	--parseBinaryOperatorExpression (exprDsc1: CallDescriptor; checkSemicolonAfter: Boolean ): ExpressionCallDescriptor is --ExprOperatorExprDescriptor is
	require
		first_expression_not_void: exprDsc1 /= Void
		valid_token: validToken (<<scanner.operator_token, scanner.less_token, scanner.greater_token, scanner.tilda_token, scanner.bar_token>>)
	local
		exprDsc2: ExpressionDescriptor
		operator: String
		cceDsc: CallChainElement
		exprCall: ExpressionCallDescriptor
		exprCall1: ExpressionCallDescriptor
		cceDsc1: CallChainElement
		order: Integer
	do
debug
	--trace ("+++parseBinaryOperatorExpression")
end
		operator := scanner.tokenString
		scanner.nextToken
		exprDsc2 := parseExpressionWithSemicolon1 (checkSemicolonAfter)
		if exprDsc2 /= Void then
			-- exprDsc1.operator(exprDsc2)
--trace ("#3: " + exprDsc1.out + " " + operator + " " + exprDsc2.out)
--trace ("#3: " + exprDsc1.out + " " + operator + "[" + getOrder (operator).out + "] " + exprDsc2.out + "[" + exprDsc2.getOrder.out + "]")
			order := exprDsc2.getOrder
			if order = 0 then
				create {CallChainElement} cceDsc.init (operator, <<exprDsc2>>)
				create {ExpressionCallDescriptor} Result.init (exprDsc1, <<cceDsc>>)
			elseif getOrder (operator) >= order then
				create {CallChainElement} cceDsc.init (operator, <<exprDsc2>>)
				create {ExpressionCallDescriptor} Result.init (exprDsc1, <<cceDsc>>)
			else
				exprCall ?= exprDsc2
				check
					non_void_expr_call: exprCall /= Void
					valid_call_chain: exprCall.callChain.count > 0
				end -- check
				create cceDsc1.init (operator, <<exprCall.expression>>)
				create exprCall1.init (exprDsc1, <<cceDsc1>>)
				create {ExpressionCallDescriptor} Result.init (exprCall1, <<exprCall.callChain.item (1)>>)
--trace ("#3.1:[" + Result.getOrder.out + "]" + Result.out)
				--create {CallChainElement} cceDsc.init (operator, <<exprDsc1>>)
				--create {ExpressionCallDescriptor} Result.init (exprDsc2, <<cceDsc>>)
			end -- if
			-- create Result.init (exprDsc1, operator, exprDsc2)
		end -- if
--trace ("---parseBinaryOperatorExpression")
	end -- parseBinaryOperatorExpression

	getOrder (identifier: String): Integer is
	-- 0. All other
	-- 1. not, ~, /=, =, ^
	-- 2. *, /, \, and, &
	-- 3. +, -, or, |
	require
		identifier_not_void: identifier /= Void and then identifier.count > 0
	do
		inspect
			identifier.item (1)
		when '~', '=', '^' then
			Result := 1
		when '*', '/', '\', '&' then
			Result := 2
		when '+', '-', '|' then
			Result := 3
		when 'n' then
			if identifier.count = 3 and then identifier.item (2) = 'o'  and then identifier.item (3) = 't' then
				Result := 1
			end -- if
		when 'a' then
			if identifier.count = 3 and then identifier.item (2) = 'n'  and then identifier.item (3) = 'd' then
				Result := 2
			end -- if
		when 'o' then
			if identifier.count = 2 and then identifier.item (2) = 'r' then
				Result := 3
			end -- if
		when 'x' then
			if identifier.count = 3 and then identifier.item (2) = 'o'  and then identifier.item (3) = 'r' then
				Result := 3
			end -- if
		when '<', '>' then
			Result := 3
		else
			-- Result := 10
		end -- inspect
	end -- getOrder

	
	parseLambdaExpression (checkSemicolonAfter: Boolean): LambdaExpression is
	-- LambdaExpression: (rtn Identifier [Signature]) | InlineLambdaExpression
	-- InlineLambdaExpression  : [pure|safe] rtn [Parameters] [“:” Type] ( [RequireBlock] InnerBlock | foreign [EnsureBlock] [end] )|(“=>”Expression )
	require
		valid_token: validToken (<<scanner.rtn_token, scanner.pure_token, scanner.safe_token>>)
	local
		name: String
		signDsc: SignatureDescriptor
		rtnDsc: UnitRoutineDeclarationDescriptor
	do
		inspect
			scanner.token
		when scanner.pure_token then
			-- InlineLambdaExpression  : pure rtn [Parameters] [“:” Type] ( [RequireBlock] InnerBlock | foreign [EnsureBlock] [end] )|(“=>”Expression )     
			--                           ^ 
			scanner.nextToken		
			if scanner.token = scanner.rtn_token then
				scanner.nextToken
				inspect
					scanner.token
				when scanner.left_paranthesis_token, scanner.require_token, scanner.foreign_token, scanner.one_line_function_token then
					rtnDsc ?= parseAnyRoutine(False, False, True, False, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
						--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
					end -- if
				else 
					if scanner.blockStart then
						rtnDsc ?= parseAnyRoutine(False, False, True, False, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
						if rtnDsc /= Void then
							create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
							--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
						end -- if
					elseif scanner.Cmode then
						syntax_error (<<
							scanner.left_paranthesis_token, scanner.require_token, scanner.left_curly_bracket_token, scanner.foreign_token, scanner.one_line_function_token
						>>)
					else					
						syntax_error (<<
							scanner.left_paranthesis_token, scanner.require_token, scanner.do_token, scanner.foreign_token, scanner.one_line_function_token
						>>)
					end -- if
				end -- inspect
			else
				syntax_error (<<scanner.rtn_token>>)
			end -- if	
		when scanner.safe_token then
			-- InlineLambdaExpression  : safe rtn [Parameters] [“:” Type] ( [RequireBlock] InnerBlock | foreign [EnsureBlock] [end] )|(“=>”Expression )     
			--                           ^ 
			scanner.nextToken		
			if scanner.token = scanner.rtn_token then
				scanner.nextToken
				inspect
					scanner.token
				when scanner.left_paranthesis_token, scanner.require_token, scanner.foreign_token, scanner.one_line_function_token then
					rtnDsc ?= parseAnyRoutine(False, False, False, True, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
						--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
					end -- if
				else 
					if scanner.blockStart then
						rtnDsc ?= parseAnyRoutine(False, False, False, True, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
						if rtnDsc /= Void then
							create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
							--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
						end -- if
					elseif scanner.Cmode then
						syntax_error (<<
							scanner.left_paranthesis_token, scanner.require_token, scanner.left_curly_bracket_token, scanner.foreign_token, scanner.one_line_function_token
						>>)
					else
						syntax_error (<<
							scanner.left_paranthesis_token, scanner.require_token, scanner.do_token, scanner.foreign_token, scanner.one_line_function_token
						>>)
					end -- if
				end -- inspect
			else
				syntax_error (<<scanner.rtn_token>>)
			end -- if	
		when scanner.rtn_token then
			scanner.nextToken
			inspect
				scanner.token
			when scanner.identifier_token then
				-- rtn Identifier [Signature]
				--     ^
				name := scanner.tokenString
				scanner.nextToken
				inspect
					scanner.token
				when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
					signDsc := parseSignature
					if signDsc /= Void then
						create {LambdaFromRoutineExpression} Result.init (name, signDsc)
					end -- if
				else
					create {LambdaFromRoutineExpression} Result.init (name, Void)
				end -- inspect
			when scanner.left_paranthesis_token, scanner.require_token, scanner.foreign_token, scanner.one_line_function_token then
				rtnDsc ?= parseAnyRoutine(False, False, False, False, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
				if rtnDsc /= Void then
					create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
					--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
				end -- if
			else
				if scanner.blockStart then
					rtnDsc ?= parseAnyRoutine(False, False, False, False, "<>", Void, Void, False, True, Void, checkSemicolonAfter or else scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create {InlineLambdaExpression} Result.init (rtnDsc.parameters,  rtnDsc.type, rtnDsc.preconditions, rtnDsc.isForeign, rtnDsc.innerBlock, rtnDsc.expr, rtnDsc.postconditions)
						--	init (p: like parameters; t: like type; pre: like preconditions; isForeign: Boolean; b: like innerBlock; e: like expr; post: like postconditions)
					end -- if
				elseif scanner.Cmode then
					syntax_error (<<
						scanner.identifier_token, scanner.left_paranthesis_token, scanner.colon_token, scanner.require_token,
						scanner.left_curly_bracket_token, scanner.foreign_token, scanner.one_line_function_token
					>>)
				else
					syntax_error (<<
						scanner.identifier_token, scanner.left_paranthesis_token, scanner.colon_token, scanner.require_token,
						scanner.do_token, scanner.foreign_token, scanner.one_line_function_token
					>>)
				end -- if
			end -- inspect
		end -- inspect
	end -- parseLambdaExpression
	
	parseReturnStatement: StatementDescriptor is
	--51
	require
		valid_start_token: validToken (<<scanner.return_token>>)
	local
		exprDsc: ExpressionDescriptor
		skipReturnCheck: Boolean
	do
		scanner.nextWithSemicolon (True)
		inspect
			scanner.token
		when scanner.semicolon_token then -- return ;
			scanner.nextToken
			create {ReturnStatementDescriptor}Result.init (Void)
		when scanner.assignment_token then
			-- return := expr
			scanner.nextToken
			exprDsc := parseExpression
			if exprDsc /= Void then
				create {AssignmentStatementDescriptor}Result.init (returnDsc, exprDsc)
			end -- if
		else -- return [Expr]
			skipReturnCheck := True
			create {ReturnStatementDescriptor}Result.init (parseOptionalExpression)
		end -- if	
		if Result /= Void and then not skipReturnCheck then
			inspect
				scanner.token
			when scanner.end_token, scanner.else_token, scanner.elsif_token, scanner.when_token, scanner.ensure_token, scanner.illegal_token then
			else
				validity_warning ( "All statements after return till end of the block will never be executed in the file `" + scanner.sourceFileName + "`")
			end -- inspect
		end -- if
	end -- parseReturnStatement

	parseRaiseStatement: RaiseStatementDescriptor is
	--52
	require
		valid_start_token: validToken (<<scanner.raise_token>>)
	do
		scanner.nextWithSemicolon (True)
		inspect
			scanner.token
		when scanner.semicolon_token then -- raise ;
			scanner.nextToken
			create Result.init (Void)
		else -- raise
			create Result.init (parseOptionalExpression)
		end -- if	
		if Result /= Void then
			inspect
				scanner.token
			when scanner.else_token, scanner.elsif_token, scanner.when_token, scanner.ensure_token, scanner.illegal_token then
			else
				if not scanner.blockEnd then
					validity_warning ( "All statements after raise till end of the block will never be executed in the file `" + scanner.sourceFileName + "`")
				end -- if
			end -- inspect
		end -- if
	end -- parseRaiseStatement

	parseDetachStatement: DetachStatementDescriptor is
	-- ? entity
	require
		valid_start_token: validToken (<<scanner.detach_token>>)
	do
		scanner.nextToken
		if scanner.token = scanner.identifier_token then
			create Result.init (scanner.tokenString)
		else
			syntax_error (<<scanner.identifier_token>>)
		end -- if	
	end -- parseDetachStatement

	parseNewExpression: NewExpressionDescriptor is
	-- NewExpression: new UnitType [ Arguments ]
	require
		valid_start_token: validToken (<<scanner.new_token>>)
	local
		utDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
	do
		scanner.nextToken
		utDsc := parseUnitType
		if utDsc /= Void then
			create Result.init (utDsc, parseArguments)
		end -- if
	end -- parseNewExpression

	parseNewStatement: NewStatementDescriptor is
	-- NewStatement: new [“{” UnitType “}”] ( identifier | return ) [ Arguments ]
	require
		valid_start_token: validToken (<<scanner.new_token>>)
	local
		unitTypeDsc: UnitTypeCommonDescriptor
		nmdDsc: NamedTypeDescriptor
		name: String
		wasError: Boolean
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.identifier_token then
			name := scanner.tokenString
			scanner.nextToken
		when scanner.return_token then
			scanner.nextToken
		when scanner.left_curly_bracket_token then
			scanner.nextToken
			nmdDsc := parseUnitType
			if nmdDsc = Void then
				wasError := True
			else
				unitTypeDsc ?= nmdDsc
				if unitTypeDsc = Void then
					validity_error ("Formal generic parameter `" + nmdDsc.name + "` can not be used as the type of object creation")
					wasError := True
				end -- if
			end -- if
			if scanner.token = scanner.right_curly_bracket_token then
				scanner.nextToken
				inspect
					scanner.token
				when scanner.identifier_token then
					name := scanner.tokenString
					scanner.nextToken
				when scanner.return_token then
					scanner.nextToken
				else
					wasError := True
					syntax_error (<<scanner.identifier_token, scanner.return_token>>)
				end -- inspect
			else
				wasError := True
				syntax_error (<<scanner.right_curly_bracket_token>>)
			end -- if
			--if not wasError then
			--	create Result.init (unitTypeDsc, name, parseArguments)
			--end -- if
		else
			wasError := True
			syntax_error (<<scanner.identifier_token, scanner.return_token, scanner.left_curly_bracket_token>>)
		end -- if	
		if not wasError then
			create Result.init (unitTypeDsc, name, parseArguments)
		end -- if
	end -- parseNewStatement
	
	parseArguments: Array [ExpressionDescriptor] is 
	do
		if scanner.token = scanner.left_paranthesis_token then
			scanner.nextToken
			Result := parseArguments1
		end -- if
	end -- parseArguments

	parseArguments1: Array [ExpressionDescriptor] is 
	-- ExpressionList: "( "Expression {“,” Expression} ")"
	local
		exprDsc: ExpressionDescriptor
		nmdDsc: NamedTypeDescriptor
		forcedType: UnitTypeCommonDescriptor
		forcedExprDsc: ForcedExpressionDescriptor
		toLeave: Boolean
		requireExpression: Boolean
	do
--trace (">>>parseArguments1")
		from
			create Result.make (1, 0)
		until
			toLeave
		loop	
			if scanner.token = scanner.left_curly_bracket_token then
				scanner.nextToken
				nmdDsc := parseUnitType
				if nmdDsc = Void then
					toLeave := True
				else
					forcedType ?= nmdDsc
					if forcedType = Void then
						-- Forced type cannot be formal generic
						validity_error ("Formal generic parameter '" + nmdDsc.name + "` cannot be used asa forced type")
						toLeave := True
					end -- if
				end -- if
				if scanner.token = scanner.right_curly_bracket_token then
					scanner.nextToken
					requireExpression := True
				else
					syntax_error (<<scanner.right_curly_bracket_token>>)
					toLeave := True
				end -- if
			else
				forcedType := Void
			end -- if
			if requireExpression then
				exprDsc := parseExpression
			else
				exprDsc := parseOptionalExpression
			end -- if
			if exprDsc = Void then
				toLeave := True
				if scanner.token = scanner.right_paranthesis_token then
					scanner.nextToken
				else
					syntax_error (<<scanner.right_paranthesis_token>>)
--trace ("#1:parseArguments")
					Result := Void
				end -- if
			else			
--trace ("#3: argument " + exprDsc.out)
				if forcedType = Void then
					Result.force (exprDsc, Result.count + 1)
				else
					create forcedExprDsc.init (forcedType, exprDsc)
					Result.force (forcedExprDsc, Result.count + 1)
				end -- if
				inspect
					scanner.token
				when scanner.comma_token then
					scanner.nextToken
					requireExpression := True
				when scanner.right_paranthesis_token then
					scanner.nextToken
					toLeave := True
				else
--trace ("#2parseArguments")
					syntax_error (<<scanner.comma_token, scanner.right_paranthesis_token>>)
					Result := Void
					toLeave := True
				end -- inspect
			end -- if
		end -- loop
		if Result /= Void and then Result.count = 0 then
			Result := Void
		end -- if
--trace ("<<<parseArguments1")
	end -- parseArguments1

	emptySignature: SignatureDescriptor is
	once
		create Result.init (Void, Void)
	end -- emptySignature
	
	parseMemberDescription: Sorted_Array [MemberDescriptionDescriptor] is
	-- MemberDescription:([rtn] RoutineName [Signature] )|( Idenitifer {“,”Idenitifer} ”:”UnitType )
	local
		rtnDDsc: RoutineDescriptionDescriptor
		atrDDsc: AttributeDescriptionDescriptor
		signDsc: SignatureDescriptor
		names: Sorted_Array [String]
		utnDsc: UnitTypeCommonDescriptor
		name: String
		toLeave: Boolean
		commaFound: Boolean
		wasError: Boolean
		i, n: Integer
	do
		inspect
			scanner.token
		when scanner.identifier_token then
			-- MemberDescription:(Identifier [Signature] )|( Idenitifer {“,”Idenitifer} ”:”UnitType )		
			create names.fill (<<scanner.tokenString>>)
			scanner.nextToken
			inspect
				scanner.token
			when scanner.comma_token then
				-- MemberDescription: Idenitifer {“,”Idenitifer} ”:”UnitType
				--                                 ^
				from
					scanner.nextToken
				until
					toLeave
				loop
					inspect 
						scanner.token
					when scanner.identifier_token then
						if commaFound or else names.count = 1 then
							if not names.added (scanner.tokenString) then
								validity_error( "Duplicated declaration of attribute `" + scanner.tokenString + "`")
								wasError := True
							end -- if
							scanner.nextToken
							commaFound := False
						else
							syntax_error (<<scanner.comma_token, scanner.colon_token>>)
							wasError := True
							toLeave := True
						end -- if
					when scanner.comma_token then
						if commaFound then
							syntax_error (<<scanner.identifier_token, scanner.colon_token>>)
							wasError := True
						else
							scanner.nextToken
							commaFound := True
						end -- if
					when scanner.colon_token then
						if commaFound then
							syntax_error (<<scanner.identifier_token>>)
							wasError := True
						else
							scanner.nextToken
						end -- if
						toLeave := True
					else
						if commaFound then
							syntax_error (<<scanner.identifier_token>>)
						else
							syntax_error (<<scanner.comma_token, scanner.colon_token>>)
						end -- if
						wasError := True
						toLeave := True
					end
				end -- loop
				if not wasError then
					utnDsc ?= parseUnitType
					if utnDsc /= Void then
						from
							i := 1
							n := names.count
							create Result.make
						until
							i > n
						loop
							create atrDDsc.init (names.item (i), utnDsc)
							Result.add (atrDDsc)
							i := i + 1
						end -- loop
					end -- if
				end -- if
			when scanner.left_paranthesis_token then
				-- MemberDescription:Identifier Signature
				--                              ^
				signDsc := parseSignature
				if signDsc /= Void then
					create rtnDDsc.init (names.item (1), signDsc)
					create Result.fill (<<rtnDDsc>>)
				end -- if
			when scanner.colon_token then
				-- MemberDescription:Identifier ”:”UnitType
				--                               ^
				--create rtnDDsc.init (Void, Void)
				scanner.nextToken
				utnDsc ?= parseUnitType
				if utnDsc /= Void then
					create atrDDsc.init (names.item (1), utnDsc)
					create Result.fill (<<atrDDsc>>)
				end -- if
			else
				syntax_error (<<scanner.comma_token, scanner.left_paranthesis_token, scanner.colon_token>>)
			end -- inspect			
		when scanner.rtn_token then
			-- MemberDescription: rtn RoutineName [Signature]
			--                    ^
			scanner.nextToken
			inspect
				scanner.token
			when scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
			then
				name := scanner.tokenString
				scanner.nextToken
				if scanner.token = scanner.left_paranthesis_token then
					-- MemberDescription:Identifier Signature
					--                              ^
					signDsc := parseSignature
					if signDsc /= Void then
						create rtnDDsc.init (name, signDsc)
						create Result.fill (<<rtnDDsc>>)
					end -- if
				else
					create rtnDDsc.init (name, emptySignature)
					create Result.fill (<<rtnDDsc>>)
				end -- if	
			else
				syntax_error (<<
					scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
				>>)
			end -- inspect
		when scanner.operator_token, -- scanner.minus_token,
			scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
		then
			-- MemberDescription:[rtn] RoutineName [Signature]
			name := scanner.tokenString
			scanner.nextToken
			if scanner.token = scanner.left_paranthesis_token then
				-- MemberDescription:Identifier Signature
				--                              ^
				signDsc := parseSignature
				if signDsc /= Void then
					create rtnDDsc.init (name, signDsc)
					create Result.fill (<<rtnDDsc>>)
				end -- if
			else
				create rtnDDsc.init (name, emptySignature)
				create Result.fill (<<rtnDDsc>>)
			end -- if			
		else
			syntax_error (<<
				scanner.identifier_token,
				scanner.rtn_token, scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
			>>)
		end -- inspect
	end -- parseMemberDescription
	
	parseIfStatementAlternatives: Array [IfStatementAlternative] is
	-- Alternatives: “:”AlternativeTags StatementsList {“:”AlternativeTags StatementsList} 
	-- 	AlternativeTags: AlternativeTag {“,” AlternativeTag}

	require	
		-- valid_alternative_start_token: validToken (<<scanner.case_token>>)
		valid_alternative_start_token: validToken (<<scanner.colon_token>>)
	local
		curTagsList: Sorted_Array [AlternativeTagDescriptor]
		tagsList: Sorted_Array [AlternativeTagDescriptor]
		altDsc: IfStatementAlternative
		isOptionalAlternative: Boolean
		toLeave : Boolean
	do
		scanner.nextToken
		from
			create Result.make (1, 0)
			create tagsList.make
			debug
				--trace (">>>parseIfStatementAlternatives")
			end -- debug
		until
			toLeave
		loop
			curTagsList := parseAlternativeTags (isOptionalAlternative, tagsList)
			if curTagsList = Void then
				toLeave := True
			else
				isOptionalAlternative := True
				create altDsc.init (curTagsList, parseStatements (False))
				Result.force (altDsc, Result.count + 1)
				-- if scanner.token = scanner.case_token then
				if scanner.token = scanner.colon_token then
					scanner.nextToken
					-- parse next alternative
				else
					toLeave := True
				end -- if					
			end -- if
		end -- loop
		if Result.count = 0 then
			Result := Void
		end -- if
--trace ("<<<parseIfStatementAlternatives")
	end -- parseIfStatementAlternatives

	parseIfExprAlternatives: Array [IfExpressionAlternative] is
	-- ExpressionAlternatives: “case” AlternativeTags Expression {“case” AlternativeTags Expression}
	require	
		--valid_alternative_start_token: validToken (<<scanner.case_token>>)
		valid_alternative_start_token: validToken (<<scanner.colon_token>>)
	local
		curTagsList: Sorted_Array [AlternativeTagDescriptor]
		tagsList: Sorted_Array [AlternativeTagDescriptor]
		altDsc: IfExpressionAlternative --AlternativeDescriptor
		isOptionalAlternative: Boolean
		toLeave : Boolean
	do
		scanner.nextToken
		from
			create Result.make (1, 0)
			create tagsList.make
		until
			toLeave
		loop
			curTagsList := parseAlternativeTags (isOptionalAlternative, tagsList)
			if curTagsList = Void then
				toLeave := True
			else
				isOptionalAlternative := True
				create altDsc.init (curTagsList, parseOptionalExpression)
				Result.force (altDsc, Result.count + 1)
				-- if scanner.token = scanner.case_token then
				if scanner.token = scanner.colon_token then
					scanner.nextToken
					-- parse next alternative
				else
					toLeave := True
				end -- if					
			end -- if
		end -- loop
		if Result.count = 0 then
			Result := Void
		end -- if
	end -- parseIfExprAlternatives

	parseAlternativeTags (isOptionalAlternative: Boolean; tagsList: Sorted_Array[AlternativeTagDescriptor]): Sorted_Array[AlternativeTagDescriptor] is
	local
		altTagDsc: AlternativeTagDescriptor
		toLeave : Boolean
		wasError: Boolean
	do
		from
			create Result.make
		until
			toLeave
		loop
			altTagDsc := parseAlternativeTag (isOptionalAlternative)
			if altTagDsc = Void then
				toLeave := True
				wasError := True -- already reported
			else
				if tagsList.added (altTagDsc) then
					Result.add (altTagDsc)
					if scanner.token = scanner.comma_token then
						scanner.nextToken
					else
						toLeave := True
					end -- if
				else
					validity_error ("Duplicated alternative " + altTagDsc.out)
					wasError := True
				end -- if
			end -- if
		end -- loop
		if Result.count = 0 or else wasError then
			Result := Void
--		elseif not wasError then
--			if scanner.Cmode then
--				if scanner.token = scanner.left_curly_bracket_token then
--					scanner.nextToken
--				else
--					syntax_error (<<scanner.left_curly_bracket_token>>)
--				end -- if
--			elseif scanner.token = scanner.do_token then
--				scanner.nextToken
--			else
--				syntax_error (<<scanner.do_token>>)
--			end -- if
		end -- if
	end -- parseAlternativeTags	

	parseAlternativeTag (isOptionalAlternative: Boolean): AlternativeTagDescriptor is
	-- Expression [“{”OperatorName ConstantExpression“}”] “..”Expression
	local
		exprDsc: ExpressionDescriptor
		lower: ExpressionDescriptor
		operator: String
		upper: ExpressionDescriptor
		utDsc: UnitTypeAlternative
	do
--trace ("%TParse alternative expression started")
		if isOptionalAlternative then
			exprDsc := parseOptionalExpressionX
		else
			exprDsc := parseExpressionX
		end -- if
		if exprDsc /= Void then
--trace ("%TParse alternative - " + exprDsc.out)
			inspect	
				scanner.token
			when scanner.period_token then
				-- Expression “..” Expression
				scanner.nextToken
				lower := exprDsc
				upper := parseExpression
				if lower /= Void and then upper /= Void then
					create {RangeAlternative} Result.init (lower, Void, Void, upper)
				end -- if
			when scanner.left_curly_bracket_token then
				scanner.nextToken
				if scanner.token = scanner.identifier_token then
					operator := scanner.tokenString
					scanner.nextToken
					lower := exprDsc
					exprDsc := parseExpression
					if exprDsc /= Void then
						if scanner.token = scanner.right_curly_bracket_token then
							scanner.nextToken
							if scanner.token = scanner.period_token then
								scanner.nextToken
								upper := parseExpression
								if lower /= Void and then upper /= Void then
									create {RangeAlternative} Result.init (lower, operator, exprDsc, upper)
								end -- if
							else
--trace ("#2")
								syntax_error (<<scanner.period_token>>)
							end -- if
						else
							syntax_error (<<scanner.right_curly_bracket_token>>)
						end -- if
					end -- if
				else
					syntax_error (<<scanner.identifier_token>>)
				end -- if
			else
				utDsc ?= exprDsc
				if utDsc = Void then
					create {ExpressionAlternative} Result.init (exprDsc) -- It is just an expression
				else
					Result := utDsc -- It is just a UnitType alternative
				end -- if
			end -- inspect
		end -- if
debug
--		if Result /= Void then
--trace ("%TAlternative expression: " +  Result.out)
--		end -- if
end
	end -- parseAlternativeTag
	
	parseModuleCall: StatementDescriptor is
	require
		valid_start_token: validToken (<<scanner.type_name_token>>)
	local
		typeName: String
		unitTypeDsc: UnitTypeNameDescriptor	
	do
		typeName := scanner.tokenString
		scanner.nextToken		
		inspect
			scanner.token
		when scanner.dot_token then
			-- Module member call in the form of the call: ident.
			--                                                  ^
			create unitTypeDsc.init (typeName, Void)
			unitTypeDsc ?= register_type (unitTypeDsc)
			Result := parseWritableCall (unitTypeDsc)
		else
			if scanner.genericsStart then
				-- parse for more Type [ .... ] .....
				--                     ^
				unitTypeDsc ?= parseUnitTypeName1 (typeName, False)
				if unitTypeDsc /= Void then
					if scanner.token = scanner.dot_token then
						Result := parseWritableCall (unitTypeDsc)				
					else
						syntax_error (<<scanner.dot_token>>)
					end -- if
				end -- if
			elseif scanner.Cmode then
				syntax_error (<<scanner.dot_token, scanner.greater_token>>)
			else
				syntax_error (<<scanner.dot_token, scanner.right_square_bracket_token>>)
			end -- if
		end -- inspect
	end -- parseModuleCall
	
	parseIfStatement: StatementDescriptor is
	do
		Result ?= parseIfStatementOrExpression (True, False)
	end -- parseIfStatement

	parseIfExpression (checkSemicolonAfter: Boolean): IfExpressionDescriptor is
	do
		Result ?= parseIfStatementOrExpression (False, checkSemicolonAfter)
	end -- parseIfExpression

	parseIfStatementOrExpression (isStatement, checkSemicolonAfter: Boolean): Any is -- StatementDescriptor is
	--56	IfCase:
	--	if Expression (is IfBody)|(do [StatementsList])
	--	{elsif Expression (is IfBody)|(do [StatementsList]) }
	--	[else [ StatementsList ]]
	--	end
	--	IfBody: (ValueAlternative “:” StatementsList {ValueAlternative “:” StatementsList} ) | ( “(” MemberDesciption {“,”} MemberDesciption “)” )
	--	ValueAlternative : Expression ([“..”Expression ] | {“|”Expression} ) {“,”Expression ([“..”Expression ] | {“|”Expression} )}
	-- 	MemberDescription : ( [rtn] RoutineName [Signature] )|( Idenitifer “:”UnitType )
	--
	-- IfExpression:
	-- if     Expression (is IfBodyExpression)|(do Expression)
	-- {elsif Expression (is IfBodyExpression)|(do Expression)}
	-- else Expression
	-- IfBodyExpression: ValueAlternative “:” Expression {ValueAlternative “:” Expression}
	
	require
		valid_start_token: validToken (<<scanner.if_token>>)
		semicolon_consistency: checkSemicolonAfter implies not isStatement
	local
		-- IfStatementDescriptor
		ifLines: Array [IfLineDecsriptor]
		elsePart: Array [StatementDescriptor]
		ifLineDsc: IfLineDecsriptor
		
		-- IfExpressionDescriptor
		--ifExprLines: Array [IfExprLineDescriptor]
		ifExprLines: Array [IfLineDecsriptor]
		elseExpr: ExpressionDescriptor
		--ifExprLineDsc: IfExprLineDescriptor
		ifExprLineDsc: IfLineDecsriptor

		-- IfLineDecsriptor
		ifExpr: ExpressionDescriptor -- COMMON

		alternatives: Array [AlternativeDescriptor] -- IS
		statements: Array [StatementDescriptor] -- DO

		exprAlternatives: Array [IfExpressionAlternative] -- IS
		doExpr: ExpressionDescriptor -- DO

		--typeOfDsc: IsAttachedDescriptor
		--alternativeTypeDsc: UnitTypeCommonDescriptor

		caseFound: Boolean
	
		toLeave: Boolean
		wasError: Boolean
	do
--trace (">>>parse_if")
		scanner.nextToken
		ifExpr := parseExpression
		if ifExpr /= Void then
debug
	--trace ("%Tif " + ifExpr.out )
end -- debug
			inspect	
				scanner.token
			--  when scanner.case_token then
			when scanner.colon_token then
			-- when scanner.is_token then
				-- if with alternatives				
				--scanner.nextToken
				caseFound := True
				if isStatement then
					alternatives := parseIfStatementAlternatives 
					if alternatives = Void then
						wasError := True
					end -- if
				else
					exprAlternatives := parseIfExprAlternatives
					if exprAlternatives = Void then
						wasError := True
					end -- if
				end -- if
				
				--if scanner.token = scanner.colon_token then
				--	isFound := True
				--	if isStatement then
				--		alternatives := parseIfStatementAlternatives
				--		if alternatives = Void then
				--			wasError := True
				--		end -- if
				--	else
				--		exprAlternatives := parseIfExprAlternatives
				--		if exprAlternatives = Void then
				--			wasError := True
				--		end -- if
				--	end -- if
				--else
				--	wasError := True
				--	syntax_error (<<scanner.colon_token>>)
				--end -- if
			else
				if scanner.blockStart then
					-- then-part detected
					scanner.nextToken
					if isStatement then
						statements := parseStatements (False)
					else
						doExpr := parseOptionalExpression
					end -- if
				else
					wasError := True
					if scanner.Cmode then
						syntax_error (<<scanner.colon_token, scanner.left_curly_bracket_token>>)
						--syntax_error (<<scanner.case_token, scanner.left_curly_bracket_token>>)
					else
						syntax_error (<<scanner.colon_token, scanner.do_token>>)
						--syntax_error (<<scanner.case_token, scanner.do_token>>)
					end -- if
				end -- if
			end -- if				
			if not wasError then
				from
					if isStatement then
						if caseFound then
							create {IfIsLineDecsriptor} ifLineDsc.init (ifExpr, alternatives)
						else
							create {IfDoLineDecsriptor} ifLineDsc.init (ifExpr, statements)
						end -- if
						ifLines := <<ifLineDsc>>
					else
						if caseFound then
							create {IfIsExprLineDescriptor} ifExprLineDsc.init (ifExpr, exprAlternatives)
						else
							create {IfDoExprLineDescriptor} ifExprLineDsc.init (ifExpr, doExpr)
						end -- if
						ifExprLines:= <<ifExprLineDsc>>
					end -- if
				until
					toLeave
				loop
					if scanner.token = scanner.elsif_token then
						scanner.nextToken
						ifExpr := parseExpression
						if ifExpr = Void then
							toLeave := True
						else
							inspect	
								scanner.token
							--when scanner.is_token then
							--when scanner.case_token then
							when scanner.colon_token then
								--scanner.nextToken
								caseFound := True
								if isStatement then
									alternatives := parseIfStatementAlternatives
									if alternatives = Void then
										wasError := True
										toLeave := True
									end -- if
								else
									exprAlternatives := parseIfExprAlternatives
									if exprAlternatives = Void then
										wasError := True
										toLeave := True
									end -- if
								end -- if								
								--if scanner.token = scanner.colon_token then
								--	isFound := True
								--	if isStatement then
								--		alternatives := parseIfStatementAlternatives
								--		if alternatives = Void then
								--			wasError := True
								--			toLeave := True
								--		end -- if
								--	else
								--		exprAlternatives := parseIfExprAlternatives
								--		if exprAlternatives = Void then
								--			wasError := True
								--			toLeave := True
								--		end -- if
								--	end -- if
								--else
								--	wasError := True
								--	toLeave := True
								--	syntax_error (<<scanner.colon_token>>)
								--end -- if
							else
								if scanner.blockStart then
									scanner.nextToken
									caseFound := False
									if isStatement then
										statements := parseStatements (False)
									else
										doExpr := parseOptionalExpression
									end -- if
								else
									wasError := True
									toLeave := True
									if scanner.Cmode then
										syntax_error (<<scanner.colon_token, scanner.left_curly_bracket_token>>)
										--syntax_error (<<scanner.case_token, scanner.left_curly_bracket_token>>)
									else
										syntax_error (<<scanner.colon_token, scanner.do_token>>)
										--syntax_error (<<scanner.case_token, scanner.do_token>>)
									end -- if
								end -- if
							end -- if				
							if not wasError then
								if isStatement then
									if caseFound then
										create {IfIsLineDecsriptor} ifLineDsc.init (ifExpr, alternatives)
									else
										create {IfDoLineDecsriptor} ifLineDsc.init (ifExpr, statements)
									end -- if
									ifLines.force (ifLineDsc, ifLines.count + 1)
								else
									if caseFound then
										create {IfIsExprLineDescriptor} ifExprLineDsc.init (ifExpr, exprAlternatives)
									else
										create {IfDoExprLineDescriptor} ifExprLineDsc.init (ifExpr, doExpr)
									end -- if
									ifExprLines.force (ifExprLineDsc, ifExprLines.count + 1)
								end -- if
							end -- if
						end -- if
					else
						toLeave := True
					end -- if
				end -- loop
				if not wasError then
					if scanner.token = scanner.else_token then
						scanner.nextToken
						if isStatement then
							elsePart := parseStatements (False)
						else
							elseExpr := parseExpressionWithSemicolon1 (checkSemicolonAfter)
						end -- if
					elseif not isStatement then
						syntax_error (<<scanner.else_token>>)
					end -- if
					if isStatement then
						if scanner.blockEnd then
							scanner.nextToken
							create {IfStatementDescriptor} Result.init (ifLines, elsePart)
						elseif scanner.Cmode then
							syntax_error (<<scanner.right_curly_bracket_token>>)
						else
							syntax_error (<<scanner.end_if_expected>>)
						end -- if
					elseif elseExpr /= Void then
						if scanner.Cmode then
							if scanner.token = scanner.right_curly_bracket_token then
								scanner.nextToken
								create {IfExpressionDescriptor} Result.init (ifExprLines, elseExpr)
							else
								syntax_error (<<scanner.right_curly_bracket_token>>)
							end -- if
						else
							create {IfExpressionDescriptor} Result.init (ifExprLines, elseExpr)
						end -- if
					end -- if
				end -- if
			end -- if
		end -- if
--trace ("<<<parse_if")
	end -- parseIfStatementOrExpression
	
	parseWhileStatement (checkForRepeatWhile: Boolean): LoopStatementDescriptor is
	--57 General Loop	: [while BooleanExpression] [RequireBlock] InnerBlock [while BooleanExpression] [EnsureBlock] end
	--57 While Loop		: while BooleanExpression [RequireBlock] InnerBlock [EnsureBlock] end
	require
		valid_start_token: validToken (<<scanner.while_token>>)
	local
		exprDsc: ExpressionDescriptor
		preconditions: Array [PredicateDescriptor]
		innerBlock: InnerBlockDescriptor
		postconditions: Array [PredicateDescriptor]
	do
		scanner.nextToken
		exprDsc := parseExpression
		if exprDsc /= Void then
			if checkForRepeatWhile then
				inspect
					scanner.token
				when scanner.ensure_token then
					create {RepeatWhileDescriptor}Result.init (exprDsc)
				else
					if scanner.blockEnd then
						create {RepeatWhileDescriptor}Result.init (exprDsc)
					end -- if
				end -- inspect
			end -- if
			if Result = Void then
				if scanner.token = scanner.require_token then
					scanner.nextToken
					preconditions := parsePredicates
				end -- if
				if scanner.blockStart then
					innerBlock := parseInnerBlock (False)
					if innerBlock /= Void then
						if scanner.token = scanner.ensure_token then
							scanner.nextToken
							postconditions := parsePredicates
						end -- if
						create Result.init (innerBlock.invariantOffList, True, exprDsc, preconditions, innerBlock.statements, innerBlock.whenClauses, innerBlock.whenElseClause, postconditions)
						if scanner.blockEnd then
							scanner.nextToken
						elseif scanner.Cmode then
							syntax_error (<<scanner.right_curly_bracket_token>>)
						else
							syntax_error (<<scanner.end_loop_expected>>)
						end -- if
					end -- if
				elseif scanner.Cmode then
					syntax_error (<<scanner.left_curly_bracket_token>>)
				else
					syntax_error (<<scanner.do_token>>)
				end -- if
			end -- if
		end -- if
	end -- parseWhileStatement

	parseStandAloneRoutine (is_pure, is_safe: Boolean): StandaloneRoutineDescriptor is
	--58
	-- [pure|safe] - all standalone rotuinmes are pure !!!! There is no global data !!!
	-- Identifier [FormalGenerics] [Parameters] [“:” Type] [EnclosedUseDirective]
	-- ^
	-- [RequireBlock]  ( ( InnerBlock [EnsureBlock] end ) | ( foreign| (“=>”Expression ) [EnsureBlock end] )

	require
		valid_token : validToken (<<scanner.identifier_token>>)
		pure_consistent: is_pure implies not is_safe
		safe_consistent: is_safe implies not is_pure
	local
		name : String
	do
		name := scanner.tokenString
		scanner.nextToken
		inspect	
			scanner.token
		when --scanner.final_token, scanner.alias_token, 
			scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token,
			scanner.require_token, scanner.one_line_function_token, scanner.use_token, scanner.foreign_token
		then
			-- Standalone routine start
--trace (">>#2")
			Result := parseStandAloneRoutine1 (is_pure, is_safe, name, Void)
		else
			if scanner.Cmode and then ( scanner.token = scanner.less_token or else scanner.token = scanner.left_curly_bracket_token)
				or else (scanner.token = scanner.left_square_bracket_token or else scanner.token = scanner.do_token)
			then
				-- Standalone routine start
--trace (">>#1")
				Result := parseStandAloneRoutine1 (is_pure, is_safe, name, Void)
			elseif scanner.Cmode then
				syntax_error (<< --scanner.final_token, scanner.alias_token, 
					scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token,
					scanner.left_curly_bracket_token,
					scanner.require_token, scanner.one_line_function_token, scanner.less_token, scanner.use_token, scanner.foreign_token
				>>)
			else
				syntax_error (<< -- scanner.final_token, scanner.alias_token, 
					scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token,
					scanner.do_token,
					scanner.require_token, scanner.one_line_function_token, scanner.left_square_bracket_token, scanner.use_token, scanner.foreign_token
				>>)
			end -- if
		end -- if
	end -- parseStandAloneRoutine

	parseUnitFunctionWithNoParametersOrLastAttributeAndInvariant (unitDsc: UnitDeclarationDescriptor; isOverriding, isFinal: Boolean; is_pure, is_safe: Boolean; name: String; returnType: TypeDescriptor): MemberDeclarationDescriptor is
	require
		return_type_not_void: returnType /= Void
		valid_token: validToken (<<scanner.require_token>>)
		current_unit_not_void: unitDsc /= Void
	local
		preconditions: Array [PredicateDescriptor]	
		dtDsc: DetachableTypeDescriptor
	do
		scanner.nextToken
		preconditions := parsePredicates
		if preconditions /= Void then
			if scanner.blockEnd then
				-- That is end of unit !!!
				unitDsc.setInvariant (preconditions)
				dtDsc ?= returnType
				if dtDsc = Void then
					create {AttachedUnitAttributeDeclarationDescriptor} Result.init (isOverriding, isFinal, False, False, name, returnType, Void, Void)
				else
					create {DetachedUnitAttributeDeclarationDescriptor} Result.init (isOverriding, isFinal, name, dtDsc.type, Void)
				end -- if
			else -- parse function!!!
				Result ?= parseAnyRoutineWithPreconditions (isOverriding, isFinal, is_pure, is_safe, name, Void, Void, Void, returnType, False, False, Void, False, preconditions, False, Void, Void, Void)
			end -- if
		end -- if
	end -- parseUnitFunctionWithNoParametersOrLastAttributeAndInvariant

	parseUnitFunctionWithNoParameters (isOverriding, isFinal: Boolean; is_pure, is_safe: Boolean; name: String; returnType: TypeDescriptor; checkSemicolonAfter: Boolean): UnitRoutineDeclarationDescriptor is
	-- when scanner.use_token, scanner.require_token, scanner.do_token, scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token then 
	require
		return_type_not_void: returnType /= Void
	do
		Result ?= parseAnyRoutine (isOverriding, isFinal, is_pure, is_safe, name, Void, returnType, False, False, Void, checkSemicolonAfter)
	end -- parseUnitFunctionWithNoParameters
	
	parseUnitRoutine (isOverriding, isFinal: Boolean; is_pure, is_safe: Boolean; name, aliasName: String; checkSemicolonAfter: Boolean): UnitRoutineDeclarationDescriptor is
	-- UnitRoutineDeclaration: [pure|safe] RoutineName [final Identifier] [Parameters] [“:” Type] [EnclosedUseDirective] ([RequireBlock] InnerBlock|virtual|foreign [EnsureBlock] [end]) | (“=>”Expression )
	--                                                 ^
	do
		Result ?= parseAnyRoutine (isOverriding, isFinal, is_pure, is_safe, name, aliasName, Void, False, False, Void, checkSemicolonAfter)
	end -- parseUnitRoutine

	parseStandAloneRoutine1(is_pure, is_safe: Boolean; name: String; type: TypeDescriptor): StandaloneRoutineDescriptor is
	do
		Result ?= parseAnyRoutine (False, False, is_pure, is_safe, name, Void, type, True, False, Void, False)
	end -- parseStandAloneRoutine1

	parseAnyRoutine(
		isOverriding, isFinal, is_pure, is_safe: Boolean; name, anAliasName: String; type: TypeDescriptor;
		isStandAlone, isLambda: Boolean; pars: Array [ParameterDescriptor]; checkSemicolonAfter: Boolean): RoutineDescriptor is
	-- StandaloneRoutine: 		[pure|safe] Identifier  [FormalGenerics] [Parameters] [“:”|"->" Type] [EnclosedUseDirective] ([RequireBlock] (InnerBlock [EnsureBlock] end)|(foreign|(“=>”Expression) [EnsureBlock end])
	-- UnitRoutineDeclaration: 	[pure|safe] RoutineName [final Identifier] [Parameters] [“:”|"->" Type] [EnclosedUseDirective]  [RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | (virtual|foreign|(“=>”Expression) [EnsureBlock end])
    --                                                  ^
	require
		name_not_void: name /= Void
		--valid_token : scanner.token = scanner.final_token or else
		--	scanner.token = scanner.alias_token or else 
		--	scanner.token = scanner.colon_token or else scanner.implies_token
		--	scanner.token = scanner.do_token or else scanner.left_curly_bracket (scanner.blockStart)
		--	scanner.token = scanner.require_token or else
		--	scanner.token = scanner.one_line_function_token or else
		--	scanner.token = scanner.left_square_bracket_token or else scanner.less_token
		--	scanner.token = scanner.left_paranthesis_token or else
		--	scanner.token = scanner.use_token or else
		--	scanner.token = scanner.foreign_token or else
		--	scanner.token = scanner.virtual_token

		pure_consistent: is_pure implies not is_safe
		safe_consistent: is_safe implies not is_pure
	local
		finalName: String
		aliasName: String
		formalGenerics: Array [FormalGenericDescriptor]
		fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]
		parameters: Array [ParameterDescriptor]
		returnType: TypeDescriptor
		ucb: UseConstBlock
		usage: Sorted_Array [EnclosedUseEementDescriptor]
		constants: Sorted_Array [UnitTypeNameDescriptor] --FullUnitNameDescriptor]
		preconditions: Array [PredicateDescriptor]
		wasError : Boolean		
	do
		if isStandAlone then
			-- standalone routine may have generics and no alias
			if scanner.genericsStart and then not isLambda then
				create fgTypes.make
				formalGenerics := parseFormalGenerics (fgTypes)
			end -- if
		else
			aliasName := anAliasName
		end -- if
		if pars /= Void then
			parameters := pars
			if scanner.token = scanner.colon_token or else scanner.token = scanner.implies_token then
				scanner.nextToken
				returnType := parseTypeDescriptor
			end -- if
			if scanner.token = scanner.use_token then
				ucb := parseEnclosedUseDirective
				if ucb /= Void then
					usage:= ucb.usage
					constants:= ucb.constants
				end -- if
			end -- if
		elseif type = Void then
debug
	--trace ("routine " + name)
end
			if scanner.token = scanner.final_token and then not isLambda then
				scanner.nextToken
				if scanner.token = scanner.identifier_token then
					finalName := scanner.tokenString
					scanner.nextToken				
				else
					syntax_error (<<scanner.identifier_token>>)
					wasError := True
				end -- if			
			end -- if
			if not isStandAlone and then scanner.token = scanner.alias_token and then not isLambda then
				scanner.nextToken
				if scanner.token = scanner.identifier_token then
					aliasName := scanner.tokenString
					scanner.nextToken				
				else
					syntax_error (<<scanner.identifier_token>>)
					wasError := True
				end -- if			
			end -- if
--			if isStandAlone and then scanner.genericsStart and then not isLambda then
--				formalGenerics := parseFormalGenerics
--			end -- if
			if scanner.token = scanner.left_paranthesis_token then
				-- scanner.nextToken
				parameters := parseParameters (isStandAlone)
			end -- if
			inspect
				scanner.token
			when scanner.colon_token, scanner.implies_token then
  				-- It is a function
				scanner.nextToken
				returnType := parseTypeDescriptor
			else
				-- It is a procedure
			end -- inspect
--			if scanner.token = scanner.colon_token  or else scanner.token = scanner.implies_token then
--				scanner.nextToken
--				returnType := parseTypeDescriptor
--			elseif scanner.token = scanner.minus_token then
--				scanner.nextToken
--				if scanner.token = scanner.greater_token then
--					-- "->"
--					scanner.nextToken
--					returnType := parseTypeDescriptor
--				else
--					syntax_error (<<scanner.greater_token>>)
--					wasError := True
--				end -- if
--			end -- if
			if scanner.token = scanner.use_token and then not isLambda then
				ucb := parseEnclosedUseDirective
				if ucb /= Void then
					usage:= ucb.usage
					constants:= ucb.constants
				end -- if
			end -- if
		else
			returnType := type
		end -- if
	
		if scanner.token = scanner.require_token then
			scanner.nextToken
			preconditions := parsePredicates
		end -- if
	
		Result := parseAnyRoutineWithPreconditions(
			isOverriding, isFinal, is_pure, is_safe, name, anAliasName, formalGenerics, fgTypes, returnType, 
			isStandAlone, isLambda, parameters, checkSemicolonAfter, preconditions, wasError, usage, constants, pars
		)
		
	end -- parseAnyRoutine
	
	parseAnyRoutineWithPreconditions (
		isOverriding, isFinal, is_pure, is_safe: Boolean; name, aliasName: String;
		formalGenerics: Array [FormalGenericDescriptor]; fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor];
		returnType: TypeDescriptor;
		isStandAlone, isLambda: Boolean; parameters: Array [ParameterDescriptor]; checkSemicolonAfter: Boolean;
		preconditions: Array [PredicateDescriptor]; we: Boolean; usage: Sorted_Array [EnclosedUseEementDescriptor];
		constants: Sorted_Array [UnitTypeNameDescriptor] ; pars: Array [ParameterDescriptor] -- FullUnitNameDescriptor]
		) : RoutineDescriptor is
	local
		isForeign: Boolean
		isVirtual: Boolean
		innerBlock: InnerBlockDescriptor
		postconditions: Array [PredicateDescriptor]
		expr: ExpressionDescriptor
		checkForEnd: Boolean
		wasError: Boolean
	do
		wasError := we
--		checkForEnd := preconditions /= Void
		if scanner.Cmode and then scanner.token = scanner.left_curly_bracket_token then
			innerBlock := parseInnerBlock (False)
			checkForEnd := True
		else
			inspect
				scanner.token
			when scanner.foreign_token then
				isForeign := True
				scanner.nextToken
			when scanner.virtual_token then
				if isLambda or else isStandAlone then
					syntax_error (<<scanner.do_token, scanner.foreign_token, scanner.one_line_function_token>>)
					wasError := True
				else
					isVirtual := True
					scanner.nextToken
				end -- if
			when scanner.one_line_function_token then
				if returnType = Void then
					validity_error ("procedure `" + name + "` cannot have function body with just one expression")
					wasError := True
				else
					--  expr				
	debug
		--trace ("Function: " + name + ": " + returnType.out + " => ")
	end -- debug
					scanner.nextToken
					expr := parseExpressionWithSemicolon
				end -- if
			when scanner.none_token then
				-- no body - no action. InnerBlock is Void
				scanner.nextToken
			else
				if scanner.blockStart then
					innerBlock := parseInnerBlock (False)
					checkForEnd := True
				elseif scanner.Cmode then
					syntax_error (<<
						scanner.left_curly_bracket_token, scanner.foreign_token, scanner.one_line_function_token, scanner.virtual_token,
						scanner.none_token
					>>)
					wasError := True
				else
					syntax_error (<<
						scanner.do_token, scanner.foreign_token, scanner.one_line_function_token, scanner.virtual_token,
						scanner.none_token
					>>)
					wasError := True
--				else 
--					-- No inner block at all !!!					
				end -- if
			end -- inspect
		end -- if
		if not wasError then
			if scanner.token = scanner.ensure_token then
				checkForEnd := True
				scanner.nextToken
				postconditions := parsePredicates
			end -- if
			if checkForEnd then 
				if scanner.blockEnd then
					scanner.nextToken
					if isStandAlone or else pars /= Void then
						create {StandaloneRoutineDescriptor} Result.init (
							is_pure, is_safe, isForeign, name, formalGenerics, fgTypes, parameters, returnType, usage, constants, preconditions, innerBlock, expr, postconditions
							-- isP, isS, isF: Boolean; aName: like name; params: like parameters; aType: like type; u: like usage; icf: like constants;
							-- pre: like preconditions; ib: like innerBlock; anexpr: like expr; post: like postconditions
						)
					else
						-- is_pure, is_safe,
						create {UnitRoutineDeclarationDescriptor} Result.init (
							isOverriding, isFinal, is_pure, is_safe, name, aliasName, parameters, returnType, usage, constants, preconditions, isForeign, isVirtual, innerBlock, expr, postconditions
						)
					end -- if
				elseif scanner.Cmode then
					syntax_error (<<scanner.right_curly_bracket_token>>)
				else
					syntax_error (<<scanner.end_routine_expected>>)
				end -- if
			elseif isStandAlone or else pars /= Void then
				create {StandaloneRoutineDescriptor} Result.init (
					is_pure, is_safe, isForeign, name, formalGenerics, fgTypes, parameters, returnType, usage, constants, preconditions, innerBlock, expr, postconditions
				)
			else
				create {UnitRoutineDeclarationDescriptor} Result.init (
					isOverriding, isFinal, is_pure, is_safe, name, aliasName, parameters, returnType, usage, constants, preconditions, isForeign, isVirtual, innerBlock, expr, postconditions
				)
			end -- if
		end -- if
		if isStandAlone then
			ast.setFGpool (Void)
		end -- if
	end -- parseAnyRoutineWithPreconditions

	parseMultiVarParameter (aResult: Array [NamedParameterDescriptor]): Array [NamedParameterDescriptor] is
	-- when scanner.comma_token then
	-- ident , ....
	--         ^
	-- scanner.token = scanner.var_token
	-- rigid ident ...
	-- ^
	require
		array_not_void: aResult /= Void and then aResult.count < 2
		consistent1: scanner.token = scanner.rigid_token implies aResult.count = 0
		consistent2: scanner.token = scanner.comma_token implies aResult.count = 1	
	local
		parDsc: NamedParameterDescriptor -- ParameterDescriptor
		typeDsc: TypeDescriptor
		wasError: Boolean
		commaFound: Boolean
		toLeave: Boolean
		i, n: Integer
	do
		from
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.rigid_token then 
				if commaFound or else aResult.count <= 1 then
					commaFound := False
					scanner.nextToken
					if scanner.token = scanner.identifier_token then
						create parDsc.init (True, scanner.tokenString, asThisType)
						aResult.force (parDsc, aResult.count + 1)
						scanner.nextToken							
					else
						syntax_error (<<scanner.identifier_token>>)
						toLeave := True
						wasError := True
					end -- if
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.identifier_token then
				if commaFound  or else aResult.count <= 1 then
					commaFound := False
					create parDsc.init (False, scanner.tokenString, asThisType)
					aResult.force (parDsc, aResult.count + 1)
					scanner.nextToken							
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.comma_token then
				if commaFound then
					syntax_error (<<scanner.var_token, scanner.identifier_token>>)
					toLeave := True
					wasError := True
				else
					commaFound := True
					scanner.nextToken
				end -- if
			when scanner.colon_token then
				if commaFound then
					syntax_error (<<scanner.var_token, scanner.identifier_token>>)
					wasError:= True
				end -- if
				toLeave := True
				scanner.nextToken
			else
				if commaFound then
					syntax_error (<<scanner.var_token, scanner.identifier_token, scanner.colon_token>>)
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
				end -- if
				toLeave := True
				wasError := True
			end -- if
		end -- loop
		if not wasError then					
			typeDsc := parseTypeDescriptorWithSemicolon
			if typeDsc /= Void then
				-- Set proper type for all parameters
				from 
					i := 1
					n := aResult.count
				until
					i > n
				loop
					aResult.item(i).setType (typeDsc)
					i := i + 1
				end -- loop
				Result := aResult
			end -- if
		end -- if
	end -- parseMultiVarParameter

	parseParameterBlock (isStandAlone: Boolean): Array [ParameterDescriptor] is
	-- Parameter: ([[rigid] Identifier{“,” [rigid] Identifier} “:” Type)|(Identifier “is” Expression|(“:=” [Identifier]))
	-- Just put them all into array!!!
	require
		valid_token: validToken (<<scanner.identifier_token, scanner.rigid_token>>)
	local
		parDsc: ParameterDescriptor
		namedParDsc: NamedParameterDescriptor
		exprDsc: ExpressionDescriptor
		typeDsc: TypeDescriptor
		name: String
	do
debug
	--trace (">> parseParameterBlock")
end -- debug
		inspect
			scanner.token
		when scanner.identifier_token then
			name := scanner.tokenString
			scanner.nextToken
			inspect
				scanner.token
			when scanner.is_token then
				-- ident is Expression
				initialisedParFound := True
				scanner.nextToken
				exprDsc := parseExpressionWithSemicolon
				if exprDsc /= Void then
					create {InitialisedParameterDescriptor} parDsc.init (name, exprDsc)
					Result := <<parDsc>>
				end -- if
			when scanner.colon_token then
				-- ident : Type
				if initialisedParFound then
					validity_error( "Initialised parameter should not be followed by the non-initilized one called `" + name + "`")
				end -- if
				scanner.nextToken
				typeDsc := parseTypeDescriptorWithSemicolon
				if typeDsc /= Void then
debug
	--trace ("Parameter: " + name + ": " + typeDsc.out)
end -- debug
					create namedParDsc.init (False, name, typeDsc)
					Result := <<namedParDsc>>
				end -- if
			when scanner.comma_token then
				-- ident , ....
				--         ^
				if initialisedParFound then
					validity_error( "Initialised parameter should not be followed by the non-initilized one called `" + name + "`")
				end -- if
				create namedParDsc.init (False, name, asThisType)
				scanner.nextToken
				Result := parseMultiVarParameter (<<namedParDsc>>)
			else
				--syntax_error (<<scanner.is_token, scanner.colon_token, scanner.comma_token>>)
				syntaxError ("Parameter definition or next parameter expected", <<scanner.is_token, scanner.colon_token, scanner.comma_token>>, 
					<<scanner.right_paranthesis_token>>)
			end -- inspect
		when scanner.rigid_token then
			-- var ident ...
			-- ^
			Result := parseMultiVarParameter (<<>>)
		end -- inspect
debug
	--trace ("parameter block parsed")
end -- debug
	end -- parseParameterBlock
	
	initialisedParFound: Boolean
	
	parseParameters (isStandAlone: Boolean): Array [ParameterDescriptor] is
	-- Parameters: “(”[Parameter{”;””|”,” Parameter}]“)”
	--              ^
	-- Parameter: ([[var] Identifier{“,” [var] Identifier} “:” Type)|(Identifier “is” Expression|(":=" [Identifier]))

	require
		valid_token: validToken (<<scanner.left_paranthesis_token>>)
	do
		initialisedParFound := False
		scanner.nextToken
		inspect 
			scanner.token
		when scanner.var_token, scanner.identifier_token, scanner.assignment_token then
			Result := parseParameters1 (isStandAlone)
		when scanner.right_paranthesis_token then
			scanner.nextToken
			Result := <<>>
		else
			syntax_error (<<scanner.var_token, scanner.identifier_token, scanner.assignment_token, scanner.right_paranthesis_token>>)
--			syntaxError ("Parameter declaration expected", <<scanner.var_token, scanner.identifier_token, scanner.assignment_token, scanner.right_paranthesis_token>>, 
--	 Incorrect followers !!!			<<scanner.right_paranthesis_token>>)
		end -- if
	end -- parseParameters

	parseParameters1 (isStandAlone: Boolean): Array [ParameterDescriptor] is
	-- Parameters: Parameter{”;””|”,” Parameter}“)”
	--             ^
	-- Parameter: ([[var] Identifier{“,” [var] Identifier} “:” Type)|(Identifier “is” Expression|(“:=” [Identifier]))
	require
		valid_token: validToken (<<scanner.var_token, scanner.identifier_token, scanner.assignment_token>>)
	local
		parBlock: Array [ParameterDescriptor]
		pars: Sorted_Array [String]
		name: String
		parDsc: AssignAttributeParameterDescriptor
		assignSingleFound: Boolean
		toLeave: Boolean
		i, n: Integer
		parFound: Boolean
		wasError: Boolean
	do
-- trace (">> parseParameters1")
		from
			create Result.make (1, 0)
			create pars.make
		until
			toLeave
		loop
debug
	if Result.count > 0 then
		--trace (">> parseParameter: " + Result.item (Result.count).out)
	end -- if
end -- debug
			inspect 
				scanner.token 
			when scanner.assignment_token then 
				parFound := True				
				scanner.nextWithSemicolon (True)
				if scanner.token = scanner.identifier_token then
					name := scanner.tokenString
					if not pars.added (name) then
						validity_error( "Duplicated parameter declaration `:= " + name + "`")
						wasError := True
					end -- if							
					if isStandAlone then
						validity_error( "Standalone rotuine should not have parameter declaration in the form of `:= " + name + "`")
						wasError := True
					end -- if
					create {AssignAttributeParameterDescriptor} parDsc.init (name) --, genAssignment)
					Result.force (parDsc, Result.count + 1)
					scanner.nextWithSemicolon (True)
				elseif assignSingleFound then
					validity_error( "Duplicated parameter declaration ':='")
					wasError := True
				else
					if isStandAlone then
						validity_error( "Standalone rotuine should not have parameter declaration in the form of `:=`")
						wasError := True
					end -- if
					assignSingleFound := True
					create {AssignAttributeParameterDescriptor} parDsc.init ("") 
					Result.force (parDsc, Result.count + 1)
				end -- if
			when scanner.identifier_token, scanner.rigid_token then 
				parFound := True
				parBlock := parseParameterBlock (isStandAlone)
				if parBlock = Void then
					toLeave := True
					wasError := True
				else
					from
						i := 1
						n := parBlock.count
					until
						i > n
					loop
						if pars.added (parBlock.item (i).name) then
							Result.force (parBlock.item (i), Result.count + 1)
						else
							validity_error( "Duplicated parameter declaration `" + parBlock.item(i).out + "`")
							wasError := True
						end -- if							
						i := i + 1							
					end -- loop 
				end -- if
			when scanner.semicolon_token, scanner.comma_token then
				if parFound then
					scanner.nextToken
					parFound := False
				else
					syntax_error (<<scanner.identifier_token, scanner.var_token, scanner.assignment_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.right_paranthesis_token then
				scanner.nextToken
				toLeave := True
			else
				if parFound then
					syntax_error (<<scanner.semicolon_token, scanner.comma_token, scanner.right_paranthesis_token>>)
				else
					syntax_error (<<scanner.identifier_token, scanner.rigid_token, scanner.assignment_token>>)
				end -- if
				toLeave := True
				wasError := True
			end -- inspect
		end -- loop
		if wasError then
			Result := Void
		end -- if
debug
	-- trace ("<< parseParameters1")
end -- debug
	end -- parseParameters1

	parseUseConst: Sorted_Array[UnitTypeNameDescriptor] is
	-- const FullUnitName {“,” FullUnitName}
	-- ^
	require
		valid_token : validToken (<<scanner.const_token>>)
	local
		nmdDsc: NamedTypeDescriptor
		astUnitDsc: UnitTypeNameDescriptor
		toLeave: Boolean
		wasError: Boolean
	do
		scanner.nextToken
		if scanner.token = scanner.type_name_token then
			from
				create Result.make
			until
				scanner.token /= scanner.type_name_token or else toLeave
			loop
				nmdDsc := parseUnitTypeName
				if nmdDsc = Void then 
					toLeave := True
				else
					astUnitDsc ?= nmdDsc
					if astUnitDsc = Void then
						validity_error ( "Importing constants from the formal generic parameter `" + nmdDsc.out + "` is not possible")
						wasError := True
					elseif not Result.added (astUnitDsc) then
						validity_warning ( "Importing constants from the unit `" + astUnitDsc.out + "` more than once") -- .name 
						-- wasError := True
					end -- if 
					if scanner.token = scanner.comma_token then
						scanner.nextToken
						if scanner.token /= scanner.type_name_token then
							syntax_error (<<scanner.type_name_token>>)
							toLeave := True
							wasError := True
						end -- if
					else
						toLeave := True
					end -- if					
				end -- if
			end -- loop
			if wasError then
				Result := Void
			end -- if				
--trace ("3# parse use const " + Result.out)
		else
			syntax_error (<<scanner.type_name_token>>)
		end -- if
	end -- parseUseConst
	
	parseUseClause is
	-- UseDirective: use (const UnitTypeName {“,” UnitTypeName}) | (AttachedType as Identifier) [“;”|newLine]
	require
		valid_token : validToken (<<scanner.use_token>>)
	local	
		constants: Sorted_Array [UnitTypeNameDescriptor]
		atDsc: AttachedTypeDescriptor
		aliasedDsc: AliasedTypeDescriptor
		aliasName: String
	do
		scanner.nextToken
		inspect	
			scanner.token
		when scanner.const_token then
			-- parse const use clause
			constants := parseUseConst
--trace ("1# use const " + constants.out)
			if constants /= Void then
				ast.setUseConst (constants)
			end -- if
		when scanner.type_name_token, scanner.rtn_token, scanner.left_paranthesis_token, 
			scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
		then 
			atDsc := parseAttachedType (False)
			if atDsc /= Void then
				if scanner.token = scanner.as_token then
					scanner.nextToken
					if scanner.token = scanner.type_name_token then
						aliasName := scanner.tokenString
						create aliasedDsc.init (aliasName, atDsc)
						aliasedDsc ?= register_type (aliasedDsc)
						atDsc.setAliasName (aliasName)
						scanner.nextToken						
					else
						syntax_error (<<scanner.type_name_token>>)
					end -- if					
				else
					syntax_error (<<scanner.as_token>>)
				end -- if
			end -- if
		else
			syntax_error (<<
				scanner.const_token, scanner.type_name_token, scanner.rtn_token, scanner.left_paranthesis_token, 
				scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
			>>)
		end -- inspect		
	end -- parseUseClause
	
	parseConstant (checkSemicolonAfter: Boolean): ConstantDescriptor is
	--  Constant : StringConstant |CharacterConstant |IntegerConstant |RealConstant
	require
		valid_token: validToken (<<scanner.string_const_token, scanner.char_const_token, scanner.integer_const_token, scanner.real_const_token>>)
	do
		inspect
			scanner.token
		when scanner.string_const_token, scanner.char_const_token then
			create Result.init (scanner.token, scanner.tokenString)
		when scanner.integer_const_token then
			create Result.init (scanner.token, scanner.integer_value)
		when scanner.real_const_token then
			create Result.init (scanner.token, scanner.tokenString.to_real)
		end -- if
		--scanner.nextToken
		scanner.nextWithSemicolon (checkSemicolonAfter)
--trace ("constant parsed: " + Result.out)		
	ensure
		non_void_constant_dsc: Result /= Void
	end -- parseConstant
	
	parseRoutineType (checkSemicolonAfter: Boolean): RoutineTypeDescriptor is
	--40 rtn [SignatureDescriptor]
	require
		valid_token: validToken (<<scanner.rtn_token>>)
	local
		signDsc: SignatureDescriptor
	do
		scanner.nextWithSemicolon (checkSemicolonAfter)
		inspect
			scanner.token
		when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
			signDsc := parseSignature1 (checkSemicolonAfter)
			if signDsc /= Void then
				create Result.init (signDsc)
			end -- if
		--when scanner.semicolon_token then
		--	scanner.nextToken
		--	create signDsc.init (Void, Void)
		--	create Result.init (signDsc)
		else
			create signDsc.init (Void, Void)
			create Result.init (signDsc)
		end -- inspect
	end

	parseSignature: SignatureDescriptor is
	do
		inspect 
			scanner.token 
		when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
			Result := parseSignature1 (False)
		else
			-- No signature
		end -- inspect
	end -- parseSignature
	
	parseSignature1 (checkSemicolonAfter: Boolean): SignatureDescriptor is
	--41 “(”[TypeDescriptor {“,” TypeDescriptor}]“)”[“:” TypeDescriptor]
	require
		valid_token: validToken (<<scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token>>)
	local
		params: Array [TypeDescriptor]
		td: TypeDescriptor
		toLeave: Boolean
		commaFound: Boolean
	do
		inspect
			scanner.token
		when scanner.left_paranthesis_token then
			from
				create params.make (1, 0)
				scanner.nextToken
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.comma_token then
					if commaFound then
						syntax_error (<<
							scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.ref_token, scanner.val_token,
							scanner.concurrent_token, scanner.rtn_token, scanner.right_paranthesis_token
						>>)
						toLeave := True
					else
						scanner.nextToken
						commaFound := True
					end -- if
				when scanner.right_paranthesis_token then
					if commaFound then
						syntax_error (<<scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.rtn_token>>)
					else
						scanner.nextWithSemicolon (checkSemicolonAfter)
					end -- if
					toLeave := True
				else
					if commaFound or else params.count = 0 then
						commaFound := False
						if checkSemicolonAfter then
							td := parseTypeDescriptorWithSemicolon
						else
							td := parseTypeDescriptor
						end -- if
						if td = Void then
							toLeave := True
						else
							--td ?= register_named_type (td)
							params.force (td, params.count +1)
						end -- if
					else
						syntax_error (<<scanner.comma_token, scanner.right_paranthesis_token>>)
						toLeave := True
					end -- if
				end -- if					
			end -- loop
			if scanner.token = scanner.colon_token or else scanner.token = scanner.implies_token then
				scanner.nextToken
				if checkSemicolonAfter then
					td := parseTypeDescriptorWithSemicolon
				else
					td := parseTypeDescriptor
				end -- if
				if td = Void then
					-- syntax_error (<<scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.rtn_token>>)
					Result := Void
				else
					--td ?= register_named_type (td)
					create Result.init (params, td)
				end -- if
			else
				create Result.init (params, Void)
			end -- if		
		when scanner.colon_token, scanner.implies_token then
			scanner.nextToken
			if checkSemicolonAfter then
				td := parseTypeDescriptorWithSemicolon
			else
				td := parseTypeDescriptor
			end -- if
			if td = Void then
				syntax_error (<<scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.rtn_token>>)
			else
				--td ?= register_type (td)
				create Result.init (Void, td)
			end -- if
		--else -- empty signature -> Void
		--	-- syntax_error (<<scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token>>)
		--	-- create Result.init (Void, Void)
		end -- inspect
		debug
			--trace ("Signature: " + Result.out)
		end -- debug
	end -- parseSignature1
	
	parseAnchorType (checkSemicolonAfter: Boolean): AnchoredCommonDescriptor is
	--43 as (this|(Identifier [Signature]))
	require
		valid_token: validToken (<<scanner.as_token>>)
	local	
		anchorName: String
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.identifier_token then 
			anchorName := scanner.tokenString
			scanner.nextWithSemicolon (checkSemicolonAfter)
			inspect
				scanner.token
			when scanner.colon_token, scanner.left_paranthesis_token then
				create {AnchorTypeDescriptor} Result.init (anchorName, parseSignature)
			else
				create {AnchorTypeDescriptor} Result.init (anchorName, Void)
			end -- inspect
		when scanner.this_token then 
			scanner.nextWithSemicolon (checkSemicolonAfter)
			Result := asThisType
		else
			syntax_error (<<scanner.identifier_token, scanner.this_token>>)
		end -- inspect
	end -- parseAnchorType
	
	asThisType: AsThisTypeDescriptor is
	once
		create Result
	end -- asThisType	
	
	parseDetachableType (checkSemicolonAfter: Boolean): DetachableTypeDescriptor is
	--64
	require
		valid_start_token: validToken (<<scanner.detach_token>>)
	local
		atd: AttachedTypeDescriptor
--pos: Integer
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.type_name_token, scanner.as_token, scanner.rtn_token, scanner.left_paranthesis_token then
			atd := parseAttachedType (checkSemicolonAfter)
			if atd /= Void then
				create Result.init (atd)
--pos := ast.typePool.seek (Result)
--if pos <= 0 then
--trace ("#4 To add `" + Result.out + "` after pos " + (-pos).out)
--end -- if
				Result ?= register_type (Result)
				check
					type_registred: Result /= Void
				end
			end -- if
		else
			syntax_error (<<scanner.type_name_token, scanner.as_token, scanner.detach_token, scanner.rtn_token, scanner.left_paranthesis_token>>)
		end -- inspect
	end -- parseDetachableType
	
	parseTupleField(tupleFields: Array [TupleFieldDescriptor]) is -- : TupleFieldDescriptor is
	-- TupleField: [Identifier {“,” Identifier}“:”] UnitType
	require
		non_void_tuple_fields: tupleFields /= Void
		valid_token: validToken (<<scanner.identifier_token>>)
	local
		names: Sorted_Array [String]
		--types: Array [UnitTypeCommonDescriptor]
		tupleField: NamedTupleFieldDescriptor
		fieldTypeDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
		toLeave: Boolean
		commaFound: Boolean
		colonFound: Boolean
		wasError: Boolean
		i, n: Integer
	do
		from
			create names.make
			--create types.make (1, 0)
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.colon_token then
				if commaFound then
					syntax_error (<<scanner.identifier_token>>)
					wasError := True
				end -- if
				scanner.nextToken
				colonFound := True
				toLeave := True
				fieldTypeDsc := parseUnitTypeWithSemicolonAfter
				if fieldTypeDsc /= Void then
					from
						i := 1
						n := names.count
					until
						i > n
					loop
						create tupleField.init (names.item (i), fieldTypeDsc)
						tupleFields.force (tupleField, tupleFields.count + 1)
						i := i + 1
					end -- loop
					--create {NamedTupleFieldDescriptor} Result.init (names, fieldTypeDsc)
				end -- if
			--when scanner.right_paranthesis_token then
			--	-- that is end of the type list!
			--	-- We keep the token to be processed at higer level!
			--	if commaFound then
			--		syntax_error (<<scanner.identifier_token>>)
			--		wasError := True
			--	elseif colonFound then
			--		syntax_error (<<scanner.type_name_token>>)
			--		wasError := True
			--	end -- if
			--	toLeave := True
			when scanner.comma_token then
				if commaFound then
					syntax_error (<<scanner.identifier_token>>)
					wasError := True
					toLeave := True
				else
					scanner.nextToken
					commaFound := True
				end -- if
			when scanner.identifier_token then
				if commaFound or else names.count = 0 then
					if not names.added(scanner.tokenString) then
						validity_error( "Duplicated tuple field name `" + scanner.tokenString + "`")
						wasError := True
					end -- if
				--	utDsc := parseUnitTypeWithSemicolonAfter
				--	if utDsc /= Void then
				--		types.force (utDsc, types.count + 1)
				--	end -- if
					scanner.nextToken		
					commaFound := False
				else
					syntax_error (<<scanner.comma_token, scanner.colon_token>>)
					wasError := True
					toLeave := True
				end -- if
			--when scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.type_name_token then
			--	-- Aha, that is type element - all names already added in were types !!!!
			--	if commaFound or else names.count = 0 then
			--		utDsc := parseUnitTypeWithSemicolonAfter
			--		if utDsc /= Void then
			--			types.force (utDsc, types.count + 1)
			--		end -- if
			--		commaFound := False
			--		--toLeave := True
			--	else
			--		syntax_error (<<scanner.comma_token, scanner.colon_token>>)
			--		wasError := True
			--		toLeave := True
			--	end -- if
			else
				if commaFound then
					syntax_error (<<scanner.identifier_token>>)
				else
					syntax_error (<<scanner.identifier_token, scanner.colon_token>>) --, scanner.right_paranthesis_token>>)
				end -- if
				wasError := True
				toLeave := True
			end -- inspect
		end -- loop
		--if not wasError then
		--	if colonFound then
		--		utDsc := parseUnitTypeWithSemicolonAfter
		--		if utDsc /= Void then
		--			create {NamedTupleFieldDescriptor} Result.init (names, utDsc)
		--			if names.count /= types.count then
		--				validity_error( "Duplicated field name in a tuple `" + Result.out + "`")
		--			end -- if
		--		end -- if
		--	else
		--		-- Just a list of types !!! in names
		--		create {ListOfTypesDescriptor} Result.init (types)
		--	end -- if
		--end -- if
--trace ("Exit from parseTupleField")
	end -- parseTupleField
	
	parseTupleType (checkSemicolonAfter: Boolean): TupleTypeDescriptor is
	--66
	-- “(”[TupleFieldDescriptor {“,”|”;” TupleFieldDescriptor}]“)”
	require
		valid_start_token: validToken (<<scanner.left_paranthesis_token>>)
	local
		--tupleFields: Array [TupleFieldDescriptor]
		--typesList: ListOfTypesDescriptor
		tupleFieldType: NamedTypeDescriptor -- UnitTypeCommonDescriptor
		tupleFieldDsc: TypedTupleFieldDescriptor
		separatorFound: Boolean
		toLeave: Boolean
		wasError: Boolean
	do
--trace (">>>parseTupleType")
		from
			create Result.init
			scanner.nextToken
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.identifier_token then
				-- that is a tuple named field (may have several names)
				if separatorFound or else Result.fields.count = 0 then
					separatorFound := False
					parseTupleField (Result.fields)
					--if tupleFields = Void then
					--	toLeave := True
					--else
					--	Result.fields.force (tupleField, Result.fields.count + 1)
					--	--typesList ?= tupleField
					--	--if typesList = Void then
					--	--	Result.fields.force (tupleField, Result.fields.count + 1)
					--	--else
					--	--	Result.fields.append (typesList.types)
					--	--end -- if
					--end -- if
				else
					syntax_error (<<scanner.comma_token, scanner.semicolon_token, scanner.right_paranthesis_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.type_name_token then
				-- Aha, that is tuple filed with no name, utDsc is just a type
				tupleFieldType := parseUnitTypeWithSemicolonAfter
				if tupleFieldType = Void then
					toLeave := True
					wasError := True					
				else
					create tupleFieldDsc.init (tupleFieldType)
					Result.fields.force (tupleFieldDsc, Result.fields.count + 1)
				end -- if
			when scanner.comma_token, scanner.semicolon_token then
				if separatorFound then
					syntax_error (<<
						scanner.identifier_token, 
						scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.type_name_token
					>>)
					toLeave := True
					wasError := True
				else
					scanner.nextToken
					separatorFound := True
				end -- if
			when scanner.right_paranthesis_token then
				-- (A, B, )  - Ok keep the last separator
				--if separatorFound then
				--	syntax_error (<<
				--		scanner.identifier_token, 
				--		scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.type_name_token
				--	>>)
				--end -- if
				scanner.nextWithSemicolon (checkSemicolonAfter)
				toLeave := True
			else
				if separatorFound then
					syntax_error (<<
						scanner.identifier_token, 
						scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token, scanner.type_name_token
					>>)
				else
					syntax_error (<<scanner.comma_token, scanner.semicolon_token, scanner.right_paranthesis_token>>)
				end -- if
				toLeave := True
				wasError := True
			end -- inspect
		end -- loop
--trace ("<<<parseTupleType")
		if wasError then
			Result := Void
		else
			Result ?= register_type (Result)
		end -- if
	end -- parseTupleType
		
	parseConstExpression (checkSemicolonAfter: Boolean): ConstExpressionDescriptor is
	local
		identDsc: IdentifierDescriptor
		constDsc: ConstantDescriptor
		cceDsc: CallChainElement 
		operator: String
	do
--trace (">>> parseConstExpression")
		inspect
			scanner.token
		when scanner.identifier_token then
-- not_implemented_yet ("parseConstExpression: <ident>")
			create identDsc.init (scanner.tokenString)
			scanner.nextWithSemicolon (checkSemicolonAfter)
			inspect
				scanner.token
			when scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token
			then
				-- ident operator
--trace ("<ident>: " + identDsc.out + " <operator>" )
				Result := parseBinaryOperatorExpression (identDsc, checkSemicolonAfter)
			when scanner.dot_token, scanner.left_paranthesis_token then
				-- ident. | ident( 
--trace ("<ident>: " + identDsc.out + " .|(" )
				Result := parseWritableCall (identDsc)
--trace ("Expr: " + Result.out)
			when scanner.semicolon_token then
				-- Just identiifer
				--	Keep semicolon!!! scanner.nextToken
--trace ("<ident>: " + identDsc.out + ";" )
				Result := identDsc
--				toExit := True
			else
--trace ("<ident>: " + identDsc.out)
				-- Just identiifer
				Result := identDsc
			end -- inspect
		when scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token then
--not_implemented_yet ("parseConstExpression: <const>")
			constDsc := parseConstant (checkSemicolonAfter)
			check
				non_void_constant_dsc: constDsc /= Void
			end
			inspect
				scanner.token
			when scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token
			then 
				Result := parseBinaryOperatorExpression (constDsc, checkSemicolonAfter)
			when scanner.semicolon_token then
				-- Just constant
				--	Keep semicolon!!! scanner.nextToken
				Result := constDsc
--				toExit:= True
			else
				-- Just constant
				Result := constDsc
--				toExit:= True
			end -- inspect
		-- when scanner.left_paranthesis_token then
			-- (ConstExpr) -- skipped sp far
		when scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token then
			-- Unary expression kind of - 5 or + 6
-- not_implemented_yet ("parseConstExpression: +<const> or -<const>")
			operator := scanner.tokenString
			scanner.nextToken
			inspect
				scanner.token
			when scanner.integer_const_token, scanner.real_const_token then
				if operator.is_equal ("+") then
					-- ignore plus sign
					Result := parseConstant (checkSemicolonAfter)				
				elseif operator.is_equal ("-") then
					-- negate the constant 
					constDsc := parseConstant (checkSemicolonAfter)
					constDsc.negate
					Result := constDsc
				else				
					constDsc := parseConstant (checkSemicolonAfter)
					check
						non_void_constant_dsc: constDsc /= Void
					end
					create {CallChainElement} cceDsc.init (operator, Void)
					create {ExpressionCallDescriptor} Result.init (constDsc, <<cceDsc>>)
				end -- if
			else
				syntax_error (<<
					scanner.integer_const_token, scanner.real_const_token
				>>)
			end -- inspect			
		else
			syntax_error (<<
				scanner.identifier_token,
				scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token,
				scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token
			>>)
		end -- inspect
--trace ("<<< parseConstExpression")
	end -- parseConstExpression

	parseRangeType (checkSemicolonAfter: Boolean): RangeTypeDescriptor is
	--67
	-- RangeType: 
	-- 	(ConstantExpression [“{”OperatorName ConstantExpression“}”] “..”ConstantExpression)
	-- 	|
	-- 	(ConstantExpression {“|” ConstantExpression})
	require
		valid_token: validToken (<<
			scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token,
			scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token			
		>>)
	local
		--expr: ExpressionDescriptor
		constExpr1: ConstExpressionDescriptor
		constExpr2: ConstExpressionDescriptor
		toLeave: Boolean
		barFound: Boolean
		--constDsc: ConstantDescriptor
		cDsc: ConstantDescriptor
		values: Array [ConstExpressionDescriptor] --ExpressionDescriptor]
		constants: Sorted_Array [ConstantDescriptor]
	do
		--constDsc := parseConstant (checkSemicolonAfter)
		constExpr1 := parseConstExpression (checkSemicolonAfter)
		if constExpr1 /= Void then
			cDsc ?= constExpr1
			if cDsc = Void then
				create constants.make
			else
				create constants.fill (<<cDsc>>)
			end -- if
			-- create constDsc.init (Void, scanner.token, scanner.tokenValue)
			--scanner.nextToken
			--create constants.fill (<<constDsc>>)
			inspect
				scanner.token
			when scanner.period_token then
				scanner.nextToken
				--expr := parseExpressionWithSemicolon1 (checkSemicolonAfter)
				constExpr2 := parseConstExpression (checkSemicolonAfter)
				if constExpr2 /= Void then
--trace ("range: " + constExpr1.out + " .. " + constExpr2.out + "`")
					cDsc ?= constExpr2
					if cDsc /= Void and then not constants.added (cDsc) then
						validity_error( "Incorrect range type `" + constExpr1.out + " .. " + constExpr2.out + "`")
					end -- if
					create {FixedRangeTypeDescriptor} Result.init (constExpr1, Void, Void, constExpr2)
				end -- if
			when scanner.bar_token then
				from
					values := <<constExpr1>> -- constDsc>>
					create {EnumeratedRangeTypeDescriptor} Result.init (values)
				until
					toLeave
				loop
					inspect
						scanner.token
					when scanner.bar_token then
						if barFound then
							toLeave := True
						else
							barFound := True
							scanner.nextToken
						end -- if
					else
						if barFound then
							constExpr2 := parseConstExpression (checkSemicolonAfter)
							if constExpr2 = Void then
								toLeave := True
							else
--trace (" bar: constant expr: " + expr.out)
-- const check does not work any more!!! paredExpression parses the whole construction ce1 | ce2 | ce ....
								cDsc ?= constExpr2 -- expr
								if cDsc /= Void and then not constants.added (cDsc) then
									validity_error( "Duplicated constant `" + cDsc.value.out + "` in range type ")
									--toLeave := True
								end -- if
								values.force (constExpr2, values.count + 1)
								barFound := False
							end -- if
						else
							toLeave := True
						end -- if
					end -- inspect
				end -- loop
				if values.count = 1 then
					validity_error( "Multi-type should have more than one type in it")
					Result := Void
				end -- if
			else
				syntax_error (<<scanner.period_token, scanner.bar_token>>)
			end -- inspect
		end -- if
	end -- parseRangeType
	
	parseMultiType (firstDsc: UnitTypeCommonDescriptor; checkSemicolonAfter: Boolean): MultiTypeDescriptor is
		-- ADT - sum
		-- MultiTypeDescriptor: UnitTypeDescriptor {“|” UnitTypeDescriptor} 
		-- not supported !!!! RangeType: ConstantExpression {“|” ConstantExpression}
		-- It could be RangeType when all identifiers are in fact constants!!!
	require
		first_element_not_void: firstDsc /= Void
		valid_token: validToken (<<scanner.bar_token>>)
	local
		nmdDsc: NamedTypeDescriptor
		typeDsc: UnitTypeCommonDescriptor
		toLeave: Boolean
		barFound: Boolean
		types: Sorted_Array [UnitTypeCommonDescriptor]
	do
		from
			create types.fill (<<firstDsc>>)
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.bar_token then
				if barFound then
					create Result.init (types)
					toLeave := True
				else
					barFound := True
					scanner.nextToken
				end -- if
			else
				if barFound or else types.count = 1 then
					inspect
						scanner.token
					when scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token then
						nmdDsc := parseUnitType1 (checkSemicolonAfter)
						if nmdDsc = Void then
							if types.count = 1 then
								validity_error( "Multi-type should have more than one type in it")
							else
								create Result.init (types)
							end -- if
							toLeave := True
						else
							typeDsc ?= nmdDsc
							if typeDsc = Void then
								validity_error( "Multi-type should not use formal genric parameter `" + nmdDsc.name + "`")
							elseif not types.added (typeDsc) then
								validity_error( "Duplicated type `" + typeDsc.name + "` within the the multi-type")
							end -- if
							barFound := False
						end -- if
					else
						syntax_error (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token>>)
						toLeave := True
					end -- inspect
				else
					if types.count = 1 then
						validity_error( "Multi-type should have more than one type in it")
					else
						create Result.init (types)
					end -- if
					toLeave := True
				end -- if
			end -- inspect
		end -- loop
	end -- parseMultiType

	parseRangeType1 (typeDsc: UnitTypeCommonDescriptor; checkSemicolonAfter: Boolean): RangeTypeDescriptor is
		-- RangeType: 
		-- 	(ConstantExpression “..” ConstantExpression)	
		--                       ^
	require
		first_elelemnt_not_void: typeDsc /= Void and then typeDsc.generics.count = 0
		valid_token: validToken (<<scanner.period_token>>)
	local
		--exprDsc: ExpressionDescriptor
		exprDsc: ConstExpressionDescriptor
		identDsc: IdentifierDescriptor
	do
		scanner.nextToken
		--exprDsc := parseExpressionWithSemicolon1 (checkSemicolonAfter)
		exprDsc := parseConstExpression (checkSemicolonAfter)
		if exprDsc /= Void then
			create identDsc.init (typeDsc.name)
			create {FixedRangeTypeDescriptor} Result.init (identDsc, Void, Void exprDsc)
		end -- if
	end -- parseRangeType1

	parseAnonymousUnitType (checkSemicolonAfter: Boolean): AnonymousUnitTypeDescriptor is
	-- AnonymousUnitType: “unit” MemberDesciption {[“;”] MemberDesciption} “end”
	--                     ^
	require
		valid_token: validToken (<<scanner.unit_token>>)
	local
		members: Sorted_Array [MemberDescriptionDescriptor]
		m1: Sorted_Array [MemberDescriptionDescriptor]
		toLeave: Boolean
		wasError: Boolean
		i, n: Integer
	do
		scanner.nextToken
		from
			create members.make
		until
			toLeave
		loop
			m1 := parseMemberDescription
			if m1 = Void then
				toLeave := True
			else
				from
					i := 1
					n := m1.count
				until
					i > n
				loop
					if not members.added (m1.item (i)) then
						validity_error( "Duplicated unit member declaration `" + m1.item (i).name + "`") 
						wasError := True
					end -- if
					i := i + 1
				end -- loop
			end -- if
		end -- loop
		if scanner.blockEnd then
			scanner.nextWithSemicolon (checkSemicolonAfter)
			if not wasError then
				create Result.init (members)
			end -- if
		elseif scanner.Cmode then
			syntax_error (<<scanner.right_curly_bracket_token>>)
		else
			syntax_error (<<scanner.end_token>>)
		end -- if
	end -- parseAnonymousUnitType

	parseAttachedType (checkSemicolonAfter: Boolean): AttachedTypeDescriptor is
	-- UnitTypeDescriptor|AnchorTypeDescriptor|MultiTypeDescriptor|TupleType|RangeTypeDescriptor|RoutineTypeDescriptor
	-- type_name          as                   identifier          (         identifier|constant rtn
	local
		nmdDsc: NamedTypeDescriptor 
		unitTypeDsc: UnitTypeCommonDescriptor
--pos: Integer
	do
debug
--	trace (">>>parseAttachedType")
end
		inspect
			scanner.token
		when scanner.type_name_token then
			-- UnitTypeDescriptor | MultiTypeDescriptor | RangeTypeDescriptor | FormalGenericType
			nmdDsc := parseUnitTypeName2 (checkSemicolonAfter)
			if nmdDsc /= Void then
				inspect
					scanner.token
				when scanner.bar_token then
					-- MultiTypeDescriptor: UnitTypeDescriptor {“|” UnitTypeDescriptor} 
					-- RangeType: ConstantExpression {“|” ConstantExpression}
					-- RangeType: 
					-- (ConstantExpression {“|” ConstantExpression})
					-- |
					-- ConstantExpression [“{”OperatorName ConstantExpression"}"] “..” ConstantExpression
-- Need to think about the new verison of RangeType
					-- It could be RangeType when all identifiers are in fact constants!!!
					unitTypeDsc ?= nmdDsc
					if unitTypeDsc = Void then
						-- G | - incorrect multi-type
						validity_error( "Multi-type should not contain formal genric parameter `" + nmdDsc.name + "`") 
					else
						Result := parseMultiType (unitTypeDsc, checkSemicolonAfter)
					end -- if
				when scanner.period_token then
					-- RangeType: 
					-- 	(ConstantExpression “..”ConstantExpression)
					-- 	|
					-- 	(ConstantExpression {“|” ConstantExpression})
					unitTypeDsc ?= nmdDsc
					if unitTypeDsc = Void then
						validity_error( "Range-type should not contain formal genric parameter `" + nmdDsc.name + "`") 
					elseif unitTypeDsc.generics.count = 0 then
						Result := parseRangeType1 (unitTypeDsc, checkSemicolonAfter)
					else
						Result := unitTypeDsc
						-- Let it be found at next step
						--syntax_error ( <<>>)
					end -- if
				--when scanner.semicolon_token then
				--	scanner.nextToken
				--	Result := typeDsc
				else
					Result := nmdDsc
				end -- inspect
			end -- if
		when scanner.ref_token, scanner.val_token, scanner.concurrent_token then
			Result := parseUnitType1 (checkSemicolonAfter)
		when scanner.as_token then
			-- AnchorTypeDescriptor
			Result := parseAnchorType (checkSemicolonAfter)
		when scanner.rtn_token then
			-- RoutineTypeDescriptor
			Result := parseRoutineType (checkSemicolonAfter)
		when scanner.left_paranthesis_token then
			-- TupleType
			Result := parseTupleType (checkSemicolonAfter)
--trace ("parseAttachedType: tuple type parsed " + Result.out)
		when scanner.unit_token then
			-- Anonymous unit type
			Result := parseAnonymousUnitType (checkSemicolonAfter)
		when scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token,
			scanner.operator_token, scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token
		then
			-- RangeType with constants
			-- 	(ConstantExpression “..”ConstantExpression)
			-- 	|
			-- 	(ConstantExpression {“|” ConstantExpression})
					-- (ConstantExpression {“|” ConstantExpression})
					-- |
					-- ConstantExpression [“{”OperatorName ConstantExpression"}"] “..” ConstantExpression
-- Need to think about the new verison of RangeType
			Result := parseRangeType (checkSemicolonAfter)
		else
			-- It is not a type! Result is Void
			syntax_error (<<
				scanner.type_name_token, 
				scanner.ref_token, scanner.val_token, scanner.concurrent_token,
				scanner.as_token,
				scanner.rtn_token,
				scanner.left_paranthesis_token,
				scanner.unit_token,
				scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token				
			>>)
		end -- inspect
		if Result /= Void then
			-- Register type in the type pool
--pos := ast.typePool.seek (Result)
--if pos <=  0 then
--trace ("#3 To add `" + Result.out + "` after pos " + (-pos).out)
--end -- if
			unitTypeDsc ?= Result
			if unitTypeDsc = Void then
				Result ?= register_type (Result)
			else
				Result ?= register_named_type (unitTypeDsc)
			end -- if
			check
				type_registred: Result /= Void
			end
		end -- if
--trace ("<<<parseAttachedType")
	end -- parseAttachedType
	
	parseTypeDescriptorWithSemicolon: TypeDescriptor is
	--39 Type: [”?”] AttachedTypeDescriptor [";"]
	do
		if scanner.token = scanner.detach_token then
			-- DetachableTypeDescriptor
			Result := parseDetachableType (True)
		else
			Result := parseAttachedType (True)
		end -- if
	end -- parseTypeDescriptorWithSemicolon
	
	parseTypeDescriptor: TypeDescriptor is
	--39 Type: [”?”] AttachedTypeDescriptor
	-- AttachedTypeDescriptor: UnitType|AnchorType|MultiType|TupleType|RangeType|RoutineType
	do
		if scanner.token = scanner.detach_token then
			-- DetachableTypeDescriptor
			Result := parseDetachableType (False)
		else
			Result := parseAttachedType (False)
		end -- if
	end -- parseTypeDescriptor
	
	parseUnitTypeName1 (name: String; checkSemicolonAfter: Boolean): NamedTypeDescriptor is -- UnitTypeNameDescriptor
	-- UnitTypeName: Identifier [“[“ (Type|ConstantExpression) {“,” ( Type|ConstantExpression)}“]” ]
	-- ConstantExpression: (Identifier {“.” Identifier}) | Constant [Operator ConstantExpression]
	require
		name_not_void: name /= Void
	local
		toLeave: Boolean
		generics: Array [TypeOrExpressionDescriptor]
		td: TypeOrExpressionDescriptor
		commaFound: Boolean
--		identifier: String
--pos: Integer
	do
debug
--	trace (">>> parseUnitTypeName1: " + name)
end
		if scanner.genericsStart then
			from
				create generics.make (1, 0)
				scanner.nextToken
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.comma_token then
--trace ("%T%TparseUnitTypeName1 " + name + "[   .... COMMA")
					if commaFound then
						syntax_error (<<scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.rtn_token>>)
						toLeave := True
					else
						commaFound := True
						scanner.nextToken
					end -- if
--				when scanner.identifier_token, scanner.type_name_token then
				when scanner.identifier_token then
					if commaFound or else generics.count = 0 then
						commaFound := False
						td := parseConstExpression (False)
						if td = Void then
							toLeave := True
						else
debug
--	trace (">>> parseUnitTypeName1: " + name + "[ident " + td.out)
end
							generics.force (td, generics.count + 1)
						end -- if
					else
						syntax_error (<<scanner.comma_token>>)
						toLeave := True
					end -- if
				when scanner.type_name_token then
					-- Type or ConstExpr
					if commaFound or else generics.count = 0 then
						commaFound := False
						td := parseTypeWithOptionalCallChain
--						identifier := scanner.tokenString
--						scanner.nextToken
--						td := parseUnitTypeName1 (identifier, False)
----						td := parseTypeDescriptor
						if td = Void then
							toLeave := True
						else
debug
--	trace (">>> parseUnitTypeName1: " + name + "[unit " + td.out)
end
							generics.force (td, generics.count + 1)
						end -- if
					else
						syntax_error (<<scanner.comma_token>>)
						toLeave := True
					end -- if
				when scanner.as_token, scanner.detach_token, scanner.rtn_token then
					-- Type!
debug
--	trace ("%T%TparseUnitTypeName1 " + name + " as|?|rtn")
end
					if commaFound or else generics.count = 0 then
						commaFound := False
						-- td := parseTypeOrConstExprDescriptor
						td := parseTypeDescriptor
						if td = Void then
							toLeave := True
						else
debug
--	trace (">>> parseUnitTypeName1: " + name + "[type " + td.out)
end
							generics.force (td, generics.count + 1)
						end -- if
					else
						syntax_error (<<scanner.comma_token>>)
						toLeave := True
					end -- if
				when scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token then
					-- Expr started with constant!
					if commaFound or else generics.count = 0 then
						commaFound := False
						--td := parseTypeOrConstExprDescriptor
						td := parseExpression
						if td = Void then
							toLeave := True
						else
debug
--	trace (">>> parseUnitTypeName1: " + name + "[const " + td.out)
end
							generics.force (td, generics.count + 1)
						end -- if
					else
						syntax_error (<<scanner.comma_token>>)
						toLeave := True
					end -- if
				else
debug
--	trace (">>> parseUnitTypeName1: finish !!! " + name)
end
					if scanner.genericsEnd then
						if commaFound then
--trace ("%T%TparseUnitTypeName1 " + name + "[   .... ] END #1")
							-- identifier, as, ?, rtn, (
							syntax_error (<<scanner.identifier_token, scanner.as_token, scanner.detach_token, scanner.rtn_token,
								scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
							>>)
						elseif generics.count = 0 then
--trace ("%T%TparseUnitTypeName1 " + name + "[   .... ] END #2")
							syntax_error (<<
								scanner.identifier_token, scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
							>>)
						else
--trace ("%T%TparseUnitTypeName1 " + name + "[   .... ] END #2")
							scanner.nextWithSemicolon (checkSemicolonAfter)
							create {UnitTypeNameDescriptor} Result.init (name, generics)
						end -- if
					else
--trace ("%T%TparseUnitTypeName1 " + name + " ???? " + scanner.tokenString)
						if commaFound then
							syntax_error (<<scanner.identifier_token, scanner.type_name_token, scanner.as_token, scanner.detach_token, scanner.rtn_token,
								scanner.integer_const_token, scanner.real_const_token, scanner.string_const_token, scanner.char_const_token
							>>)
						elseif scanner.Cmode then
							syntax_error (<<scanner.comma_token, scanner.greater_token>>)
						else
							syntax_error (<<scanner.comma_token, scanner.right_square_bracket_token>>)
						end -- if
					end -- if
					toLeave := True
				end -- inspect
			end -- loop
		else
			create {UnitTypeNameDescriptor} Result.init (name, Void)
		end -- inspect
		if Result /= Void then
			-- Register type in the type pool
--pos := ast.typePool.seek (Result)
--if pos <=  0 then
--trace ("#2 To add `" + Result.out + "` after pos " + (-pos).out)
--end -- if
			if generics = Void then
				Result ?= register_named_type (Result)
			else
				Result ?= register_type (Result)
			end -- if
			check
				type_registred: Result /= Void
			end -- check
		end -- if
--trace ("<<< parseUnitTypeName1")
	end -- parseUnitTypeName1
	
	parseTypeWithOptionalCallChain: TypeOrExpressionDescriptor is
	-- UnitType [“.”CallChain]
	require
		valid_token: validToken (<<scanner.type_name_token>>)
	local
		typeName: String
		unitTypeDsc: UnitTypeNameDescriptor
		nmdTypeDsc: NamedTypeDescriptor
	do
		-- ModuleName.callchain
		typeName := scanner.tokenString
		scanner.nextToken
		inspect
			scanner.token
		when scanner.dot_token then
			-- Module member call in the form of the call: ident.
			--                                                  ^
			create unitTypeDsc.init (typeName, Void)
			nmdTypeDsc ?= register_named_type(unitTypeDsc)
			check
				type_registered: nmdTypeDsc /= Void
			end -- check
			Result := parseWritableCall (nmdTypeDsc)
		else
			if scanner.genericsStart then
				-- parse for more Type [ .... ] .....
				--                     ^
				nmdTypeDsc := parseUnitTypeName1 (typeName, False)
				if nmdTypeDsc /= Void then
					if scanner.token = scanner.dot_token then
						Result := parseWritableCall (nmdTypeDsc)
					else
						Result := nmdTypeDsc
					end -- if
				end -- if
			else
				create {UnitTypeNameDescriptor} Result.init (typeName, Void)				
			end -- if
		end -- inspect
--trace ("%T%T%TparseTypeOrConstExprDescriptor: " + Result.out)
	end -- parseTypeWithOptionalCallChain

	--parseUnitTypeName2(checkSemicolonAfter: Boolean): UnitTypeNameDescriptor is
	--do
	--	Result := parseUnitTypeName2 (checkSemicolonAfter)	
	--end -- parseUnitTypeName2

	parseUnitTypeName: NamedTypeDescriptor is -- UnitTypeNameDescriptor is
	do
		Result := parseUnitTypeName2 (False)
	end -- parseUnitTypeName
	
	parseUnitTypeName2(checkSemicolonAfter: Boolean): NamedTypeDescriptor is -- UnitTypeNameDescriptor
	local
		name: String
		utnDsc: UnitTypeNameDescriptor
	do
		if scanner.token = scanner.type_name_token then
			name := scanner.tokenString
--trace (">>>parseUnitTypeName2: " + name + " ; - " + checkSemicolonAfter.out)
			scanner.nextWithSemicolon (checkSemicolonAfter)
			if checkSemicolonAfter and then scanner.token = scanner.semicolon_token then
				--scanner.nextToken
				create utnDsc.init (name, Void)
				-- Register type in the type pool
				Result ?= register_named_type (utnDsc)
				check
					type_registred: Result /= Void
				end -- check
			else
				Result := parseUnitTypeName1 (name, checkSemicolonAfter)
			end -- if
--trace ("<<<parseUnitTypeName2: " + Result.out)
		end -- if
	end -- parseUnitTypeName2
	
	parseUnitType: NamedTypeDescriptor is -- UnitTypeCommonDescriptor is
	do
		Result := parseUnitType2 (False)
		--inspect
		--	scanner.token
		--when scanner.type_name_token then
		--	Result := parseUnitType1 (False)
		--when scanner.ref_token, scanner.val_token, scanner.concurrent_token then
		--	Result := parseUnitType1 (False)
		--else
		--	syntax_error (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token>>)			
		--end -- inspect
	end -- parseUnitType

	parseUnitType2 (checkSemicolonAfter: Boolean): NamedTypeDescriptor is -- UnitTypeCommonDescriptor is
	do
		inspect
			scanner.token
		when scanner.type_name_token then
			Result := parseUnitType1 (checkSemicolonAfter)
		when scanner.ref_token, scanner.val_token, scanner.concurrent_token then
			Result := parseUnitType1 (checkSemicolonAfter)
		else
			syntax_error (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token>>)			
		end -- inspect
	end -- parseUnitType2

	parseUnitType1 (checkSemicolonAfter: Boolean): NamedTypeDescriptor is -- UnitTypeCommonDescriptor
	require
		valid_token: validToken (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token>>)
	local
		isRef,
		isVal,
		isConcurrent : Boolean
		nmdDsc: NamedTypeDescriptor
		utd: UnitTypeNameDescriptor
		justType: Boolean
	do
		inspect
			scanner.token
		when scanner.ref_token then
			isRef := True
			scanner.nextToken
		when scanner.val_token then
			isVal := True
			scanner.nextToken
		when scanner.concurrent_token then
			isConcurrent := True
			scanner.nextToken
		else
			justType := True
		end -- inspect
		if scanner.token = scanner.type_name_token then
			setConstructionStart
			nmdDsc := parseUnitTypeName2(checkSemicolonAfter)
			--utd := parseUnitTypeName2(checkSemicolonAfter)
			if nmdDsc /= Void then
				--create Result.init (isRef, isVal, isConcurrent, utd.name, utd.generics)
				if justType then
					Result := nmdDsc
				else
					utd ?= nmdDsc
					--check
					--	valid_type: utd /= Void
					--end -- check
					if utd = Void then
						-- ref|val|concurrent FormalGenericName
						if isRef then
							validity_error ("Formal generic parameter cannot be used as `ref " + nmdDsc.name + "`")
						elseif isVal then
							validity_error ("Formal generic parameter cannot be used as `val " + nmdDsc.name + "`")
						else -- isConcurrent
							validity_error ("Formal generic parameter cannot be used as `concurrent " + nmdDsc.name + "`")
						end -- if
					else
						create {UnitTypeDescriptor} Result.init (isRef, isVal, isConcurrent, utd.name, utd.generics)
						Result ?= register_type (Result)
						check
							type_registred: Result /= Void
						end -- check
					end -- if
				end -- if
			end -- if
			clearConstructionStart			
		else
			syntax_error (<<scanner.type_name_token>>)
		end -- if
	end -- parseUnitType1


	register_named_type (nmdType: NamedTypeDescriptor): TypeDescriptor is
	require
		non_void_type: nmdType /= Void
	local
		fgtDsc: FormalGenericTypeNameDescriptor
		unitTypeDsc: UnitTypeCommonDescriptor
	do
		unitTypeDsc ?= nmdType
		if ast.fgTypes /= Void and then unitTypeDsc /= Void and then unitTypeDsc.generics.count = 0 then
			create fgtDsc.init (nmdType.name)
			fgtDsc ?= ast.typePool.search (fgtDsc)
			if fgtDsc = Void then
				Result := ast.typePool.add_it (nmdType)
			else
debug
--	print ("Registering `" + nmdType.out + "` it is Formal generic type !!!%N")
end -- debug
				Result := fgtDsc
			end -- if
		else
			Result := ast.typePool.add_it (nmdType)
		end -- if
	ensure
		non_void_registered_type: Result /= Void		
	end -- register_named_type

	register_type (aType: TypeDescriptor): TypeDescriptor is
	require
		non_void_type: aType /= Void
	do
		Result := ast.typePool.add_it (aType)
debug
	--trace ("Registering type: " + Result.out)
end
	ensure
		non_void_registered_type: Result /= Void		
	end -- register_type
	
	parseUnitTypeWithSemicolonAfter: NamedTypeDescriptor is -- UnitTypeCommonDescriptor is
	do
		inspect
			scanner.token
		when scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token then
			Result := parseUnitType1 (True)
		else
			syntax_error (<<scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token>>)			
		end -- inspect
	end -- parseUnitTypeWithSemicolonAfter
	
	parseFactualGenericType: TypeOrExpressionDescriptor is
	require
		valid_token: validToken (<<scanner.type_name_token, scanner.identifier_token, scanner.as_token, scanner.rtn_token>>)
	local
		typeDsc: NamedTypeDescriptor -- UnitTypeNameDescriptor
		exprDsc: ExpressionDescriptor
		name: String
	do
		name := scanner.tokenString
		inspect
			scanner.token
		when scanner.type_name_token then		
			typeDsc := parseUnitTypeName
			if typeDsc /= Void then
				Result := typeDsc
			end -- if		
		when scanner.identifier_token then
			exprDsc := parseExpression
			if exprDsc /= Void then
				Result := exprDsc
			end -- if		
		end -- inspect		
	end -- parseFactualGenericType
	
	parseFormalGenericType (fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]): FormalGenericDescriptor is
	-- Identifier|TypeName ([“extend” UnitTypeName ] [“new” [Signature]])| [“:” (UnitTypeDescriptor | RoutineType)]
	-- ^
	require
		valid_token: validToken (<<scanner.type_name_token, scanner.identifier_token>>)
	local
		utnDsc: NamedTypeDescriptor -- UnitTypeNameDescriptor
		name: String
		signDsc: SignatureDescriptor
		tDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
		rtnTypeDsc: RoutineTypeDescriptor
		tupleTypeDsc: TupleTypeDescriptor
		fgtnDsc: FormalGenericTypeNameDescriptor
		wasError: Boolean
	do
		name := scanner.tokenString
		inspect
			scanner.token
		when scanner.type_name_token then
			scanner.nextToken
			inspect
				scanner.token
			when scanner.extend_token then
				scanner.nextToken
				utnDsc := parseUnitTypeName
				if utnDsc = Void then
					wasError := True
				else
					if scanner.token = scanner.new_token then
						scanner.nextToken
						inspect
							scanner.token
						when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
							signDsc := parseSignature
							wasError := signDsc = Void
						else
							create signDsc.init (Void, Void)
						end -- inspect
					end -- if
				end -- if
			when scanner.new_token then
				scanner.nextToken
				inspect
					scanner.token
				when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
					signDsc := parseSignature
					wasError := signDsc = Void
				else
					create signDsc.init (Void, Void)
				end -- inspect
			else
			end -- inspect
			if not wasError then
				create fgtnDsc.init (name)
				fgtnDsc ?= register_type (fgtnDsc)
				check
					type_registered: fgtnDsc /= Void
				end -- check
				if fgTypes /= Void then
					fgTypes.add (fgtnDsc)
				end -- if
				if utnDsc = Void and then signDsc = Void then
					Result := fgtnDsc
				else
					create {FormalGenericTypeDescriptor}Result.init (name, utnDsc, signDsc)
				end -- if
			end -- if
		when scanner.identifier_token then
			scanner.nextToken
			if scanner.token = scanner.colon_token then
				scanner.nextToken
				inspect
					scanner.token
				when scanner.type_name_token then
					-- Identifier “:” UnitTypeNameDescriptor
					tDsc := parseUnitTypeName
					if tDsc /= Void then
						create {FormalGenericConstantDescriptor} Result.init (name, tDsc)
					end -- if
				when scanner.ref_token, scanner.val_token, scanner.concurrent_token then
					-- Identifier “:” UnitTypeDescriptor
					tDsc := parseUnitType
					if tDsc /= Void then
						create {FormalGenericConstantDescriptor} Result.init (name, tDsc)
					end -- if
				when scanner.rtn_token then
					-- Identifier “:” RoutineType
					rtnTypeDsc := parseRoutineType (False)
					if rtnTypeDsc /= Void then
						create {FormalGenericRoutineDescriptor} Result.init (name, rtnTypeDsc)
					end -- if
				when scanner.left_paranthesis_token then
					-- tuple type expected
					tupleTypeDsc := parseTupleType (False)
					if tupleTypeDsc /= Void then
						create {FormalGenericConstantDescriptor} Result.init (name, tupleTypeDsc)				
	--trace ("parseFormalGenericType: tuple type parsed " + Result.out)
					end -- if				
				else
					syntax_error (<<
						scanner.type_name_token, scanner.ref_token, scanner.val_token, scanner.concurrent_token,
						scanner.rtn_token, scanner.left_paranthesis_token
					>>)
				end -- inspect
			else
				syntax_error (<<scanner.colon_token>>)
			end -- inspect
		end -- inspect		
	end -- parseFormalGenericType

--	theSameUnit2 (typeDsc: TypeOrExpressionDescriptor; fgtDsc: FormalGenericDescriptor): Boolean is
--	require
--		formal_generic_descriptor_not_void: fgtDsc /= Void
--		type_descriptor_not_void: typeDsc /= Void	
--	local
--		td: UnitTypeCommonDescriptor
--	do
--		td ?= typeDsc
--		if td /= Void then
--			Result := td.name.is_equal (fgtDsc.name) -- they have the same names ....
----			if Result then
---- not_implemented_yet ("validity check of type and formal generic")
----			end -- if
--		end -- if
--	end -- theSameUnit2

	theSameUnit1 (unitDsc: UnitDeclarationDescriptor; nmdTypeDsc: NamedTypeDescriptor) : Boolean is -- UnitTypeNameDescriptor
	require
		unit_descriptor_not_void: unitDsc /= Void
		type_descriptor_not_void: nmdTypeDsc /= Void	
	local
		utnDsc: UnitTypeNameDescriptor
		--i, n: Integer
	do
		utnDsc ?= nmdTypeDsc
		if utnDsc = Void then
			Result := unitDsc.name.is_equal (nmdTypeDsc.name) and then unitDsc.formalGenerics.count = 0
		else
			Result := unitDsc.name.is_equal (utnDsc.name) and then utnDsc.generics.count = unitDsc.formalGenerics.count
		end -- if
		--if Result then
		--	-- check generics!!!
		--	n := utnDsc.generics.count -- : Array [TypeOrExpressionDescriptor]
		--	Result := n = unitDsc.formalGenerics.count
			--if Result then  -- Array [FormalGenericDescriptor]
			--	from
			--		i := 1
			--	until
			--		i > n
			--	loop
			--		if theSameUnit2 (utnDsc.generics.item (i), unitDsc.formalGenerics.item (i)) then
			--			i := i + 1
			--		else
			--			i := n + 1
			--			Result := False
			--		end -- if
			--	end -- if
			--end -- if
		--end -- if
	end -- theSameUnit
	
	parseInheritanceClause (unitDsc: UnitDeclarationDescriptor) is
	-- extend ParentDescriptor {“,” ParentDescriptor}
	-- [“~”] UnitTypeNameDescriptor 
	--  ^^^^ obsolete 
	-- InheritDirective: extend Parent {“,” Parent} 
	-- Parent: UnitTypeName | (“~” UnitTypeName [“(”MemberName{“,”MemberName}“)”])
	-- MemberName: Identifier|(RoutineName [Signature])
	require
		valid_token: validToken (<<scanner.extend_token>>)
		unit_descriptor_not_void: unitDsc /= Void
	local
		parentDsc: ParentDescriptor
		utnDsc: UnitTypeNameDescriptor 
		toLeave: Boolean
		--commaFound: Boolean
	do
		from
			scanner.nextToken
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.tilda_token then
				--if commaFound or else unitDsc.parents.count = 0 then
				--	commaFound := False
					scanner.nextToken
					if scanner.token = scanner.type_name_token then
						if currentUnitDsc /= Void and then currentUnitDsc.hasFormalGenericParameter (scanner.tokenString) then
							-- attempt to override a member of generic parameter
							validity_error(
								"Generic parameter name `" + scanner.tokenString + "` can not be used as a non-conformant parent unit `" + currentUnitDsc.fullUnitName + "`"
							)
							scanner.nextToken
							toLeave := True
						else
							utnDsc ?= parseUnitTypeName
							if utnDsc = Void then
								toLeave := True
							else
								if theSameUnit1 (unitDsc, utnDsc) then
									validity_error( "Attempt to inherit from itself. Extending unit `" + utnDsc.out + "` in unit `" + unitDsc.name + "`")
									--toLeave := True
									-- Inheritance graph simple cycle
								else
									if scanner.token = scanner.left_paranthesis_token then
not_implemented_yet ("extend ~Parent “(”MemberName{“,”MemberName}“)” ")
									end -- if
									create parentDsc.init (True, utnDsc)
									if not unitDsc.parents.added (parentDsc) then
										validity_error( "Duplicated inheritance from unit `" + parentDsc.out + "` in unit `" + unitDsc.name + "`")
										--toLeave := True
										-- Repeated inheritance is prohibited
									end -- if
								end -- if
								if scanner.token = scanner.comma_token then
									scanner.nextToken
								else
									toLeave := True
								end -- if
							end -- if
						end -- if
					else
						syntax_error (<<scanner.type_name_token>>)
						toLeave := True
					end -- if
				--else
				--	syntax_error (<<scanner.comma_token>>)
				--	toLeave := True
				--end -- if
			when scanner.type_name_token then
				--if commaFound or else unitDsc.parents.count = 0 then
				--	commaFound := False					
					if currentUnitDsc /= Void and then currentUnitDsc.hasFormalGenericParameter (scanner.tokenString) then
						-- attempt to override a member of generic parameter
						validity_error( "Generic parameter name `" + scanner.tokenString + "` can not be used as a parent unit `" + currentUnitDsc.fullUnitName + "`")
						scanner.nextToken
						toLeave := True
					else
						utnDsc ?= parseUnitTypeName
						if utnDsc = Void then
							toLeave := True
						else
							if theSameUnit1 (unitDsc, utnDsc) then
								validity_error( "Attempt to inherit from itself. Extending unit `" + utnDsc.out + "` in unit `" + unitDsc.name + "`")
								--toLeave := True
								-- Inheritance graph simple cycle
							else
								create parentDsc.init (False, utnDsc)
								if not unitDsc.parents.added (parentDsc) then
									validity_error( "Duplicated inheritance from unit `" + parentDsc.out + "` in unit `" + unitDsc.name + "`")
									--toLeave := True
									-- Repeated inheritance is prohibited
								end -- if
							end -- if
							if scanner.token = scanner.comma_token then
								scanner.nextToken
							else
								toLeave := True
							end -- if
						end -- if
					end -- if		
				--else
				--	--syntax_error (<<scanner.comma_token>>)
				--	toLeave := True
				--end -- if
			--when scanner.comma_token then
			--	if commaFound or else unitDsc.parents.count = 0 then
			--		syntax_error (<<scanner.identifier_token, scanner.tilda_token>>)
			--		toLeave := True
			--	else
			--		commaFound := True
			--		scanner.nextToken
			--	end -- if
			else
				--if commaFound then
					syntax_error (<<scanner.type_name_token, scanner.tilda_token>>)
				--end -- if
				toLeave := True
			end -- inspect
		end -- loop
	end -- parseInheritanceClause

	parseEnclosedUseDirective: UseConstBlock is
	--71
	-- EnclosedUseDirective: [use [EnclosedUseEement {“,” EnclosedUseEement}] [const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}]]
	-- usage: Sorted_Array [EnclosedUseEementDescriptor]
	-- constants: Sorted_Array [FullUnitNameDescriptor]
	require
		valid_token: validToken (<<scanner.use_token>>)
		-- unit_descriptor_not_void: unitDsc /= Void
	local
		usage: Sorted_Array [EnclosedUseEementDescriptor]
		constants: Sorted_Array [UnitTypeNameDescriptor] --FullUnitNameDescriptor]
		utnd: UnitTypeNameDescriptor
		eueDsc: EnclosedUseEementDescriptor
		toLeave: Boolean
		commaFound: Boolean
		wasError: Boolean
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.const_token then
			-- use const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
			-- add to -> constants: Sorted_Array [FullUnitNameDescriptor]
			constants := parseUseConst
--trace ("4# parseEnclosedUseDirective constants: " + constants.out)			
		when scanner.type_name_token then
			-- use [EnclosedUseEement {“,” EnclosedUseEement}] [const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
			-- 		UnitTypeNameDescriptor [as Identifier]]
			from
				create usage.make
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.comma_token then
					if usage.count = 0 then
						syntax_error (<<scanner.type_name_token>>)
						toLeave := True
						wasError := True
					else
						commaFound := True
						scanner.nextToken
					end -- if
				when scanner.type_name_token then
					commaFound := False
					if currentUnitDsc /= Void and then currentUnitDsc.hasFormalGenericParameter (scanner.tokenString) then
						-- attempt to override a member of generic parameter
						validity_error( "Generic parameter name `" + scanner.tokenString + "` can not be used in the use clause in unit `" + currentUnitDsc.fullUnitName + "`")
						scanner.nextToken
						toLeave := True
						wasError := True
					else
						utnd ?= parseUnitTypeName
						if utnd = Void then
							toLeave := True
							wasError := True
						else
							inspect
								scanner.token
							when scanner.comma_token then 
								-- use [EnclosedUseEement {“,” EnclosedUseEement}] [const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
								--                          ^
								-- 		UnitTypeNameDescriptor [as Identifier]]
								create eueDsc.init (utnd, Void)
								if not usage.added (eueDsc) then
									validity_error( "Duplicated import of consntants from unit `" + eueDsc.out + "`") --  in unit `" + unitDsc.name + "`")
									--toLeave := True
									wasError := True
								else
									commaFound := True
								end -- if
								scanner.nextToken
							when scanner.as_token then
								scanner.nextToken
								if scanner.token = scanner.type_name_token then
									if currentUnitDsc /= Void and then currentUnitDsc.hasFormalGenericParameter (scanner.tokenString) then
										-- attempt to override a member of generic parameter
										validity_error(
											"Generic parameter name `" + scanner.tokenString + "` can not be used as new name for renaming in unit `" +
											currentUnitDsc.fullUnitName + "`"
										)
										scanner.nextToken
										toLeave := True
										wasError := True
									else
										create eueDsc.init (utnd, scanner.tokenString)
										if not usage.added (eueDsc) then
											if currentUnitDsc = Void then
												validity_error( "Duplicated import of constants from `" + eueDsc.out + "`")
											else
												validity_error( "Duplicated import of constants from `" + eueDsc.out + "` in unit `" + currentUnitDsc.name + "`")
											end -- if
											wasError := True
										end -- if
										scanner.nextToken
										commaFound := False
									end -- if
								else
									syntax_error (<<scanner.type_name_token>>)
									toLeave := True
									wasError := True
								end -- if
							else
								-- use [EnclosedUseEement {“,” EnclosedUseEement}] [const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
								--                          ^
								-- 		UnitTypeNameDescriptor [as Identifier]]
								create eueDsc.init (utnd, Void)
								if not usage.added (eueDsc) then
									validity_warning ( "Duplicated import of consntants from unit `" + eueDsc.out + "`") --  in unit `" + unitDsc.name + "`")
									--wasError := True
								end -- if
								toLeave := True
							end -- inspect
						end -- if
					end -- if			
				when scanner.const_token then 
					if commaFound then
						syntax_error (<<scanner.type_name_token>>)
						wasError := True
					end -- if
					-- use EnclosedUseEement const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
					--                       ^
					-- 		UnitTypeNameDescriptor [as Identifier]]
					constants := parseUseConst
--trace ("2# parseEnclosedUseDirective use const " + constants.out)
					toLeave := True
				else
					syntax_error (<<scanner.comma_token, scanner.const_token, scanner.as_token>>)
					toLeave := True
					wasError := True
				end -- inspect		
			end -- loop			

			if scanner.token = scanner.const_token then
				-- use [EnclosedUseEement {“,” EnclosedUseEement}] const FullUnitNameDescriptor {“,” FullUnitNameDescriptor}
				--                                                 ^
				-- 		UnitTypeNameDescriptor [as Identifier]]
				constants := parseUseConst
--trace ("3# parseEnclosedUseDirective use const " + constants.out)
			end -- if
		else
			syntax_error (<<scanner.type_name_token, scanner.const_token>>)
			wasError := True
		end -- inspect	
		if not wasError then
--trace ("5# parseEnclosedUseDirective use const " + constants.out)
			create Result.init (usage, constants)
		end -- if			
	end -- parseEnclosedUseDirective
	
	privateDsc: PrivateVisibilityDescriptor is
	once
		create Result
	end -- privateDsc
	
	noneDsc:  NoneVisibilityDescriptor is
	once
		create Result
	end -- noneDsc
	
	anyDsc: AnyVisibilityDescriptor is
	once
		create Result
	end -- anyDsc
	
	parseMemberVisibility (unitDsc: UnitDeclarationDescriptor): MemberVisibilityDescriptor is
	--18 “{” [this| UnitTypeNameDescriptor {“,” UnitTypeNameDescriptor}  ] “}”
	require
		valid_token: validToken (<<scanner.left_curly_bracket_token, scanner.left_square_bracket_token>>)
	local
		toLeave: Boolean
		utnDsc: UnitTypeNameDescriptor
		commaFound : Boolean
		svDsc: SelectedVisibilityDescriptor
		anyFound: Boolean
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.this_token then
			scanner.nextToken
			if scanner.visibilityEnd then
				-- {this}
				--create {PrivateVisibilityDescriptor}Result.init
				Result := privateDsc
				scanner.nextToken
			elseif scanner.Cmode then
				syntax_error (<<scanner.right_square_bracket_token>>)
			else
				syntax_error (<<scanner.right_curly_bracket_token>>)
			end -- iif
		when scanner.type_name_token then
			from
				create svDsc.init -- (Void)
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.type_name_token then
					if commaFound or else svDsc.clients.count = 0 then
						commaFound := False
						if unitDsc.hasFormalGenericParameter (scanner.tokenString) then
							-- attempt to override a member of generic parameter
							validity_error(
								"Generic parameter name `" + scanner.tokenString + "` can not be listed in clients list in unit `" + 
								unitDsc.fullUnitName + "`"
							)
							scanner.nextToken
							toLeave := True
						else
							utnDsc ?= parseUnitTypeName
							if utnDsc = Void then
								toLeave := True
							else
								if utnDsc.name.is_equal ("Any") then
									anyFound := True
								elseif not anyFound then
									svDsc.clients.add (utnDsc)
								end -- if
							end -- if
						end -- if
					else
						syntax_error (<<scanner.comma_token, scanner.right_curly_bracket_token>>)
					end -- if
				when scanner.comma_token then 
					if commaFound then
						syntax_error (<<scanner.type_name_token>>)
					else
						scanner.nextToken
						commaFound := True
					end -- if
				else
					if scanner.visibilityEnd then
						if commaFound then
							syntax_error (<<scanner.type_name_token>>)
						else
							scanner.nextToken
							Result := svDsc
						end -- if
						toLeave := True
					else
						if commaFound then
							if scanner.Cmode then
								syntax_error (<<scanner.type_name_token, scanner.right_square_bracket_token>>)
							else
								syntax_error (<<scanner.type_name_token, scanner.right_curly_bracket_token>>)
							end -- if
						else
							syntax_error (<<scanner.comma_token>>)
						end -- if
						toLeave := True
					end -- if
				end -- inspect
			end -- loop
			if anyFound then
				Result := anyDsc
			end -- if
		else
			if scanner.visibilityEnd then
				-- {} 
				Result := noneDsc
				scanner.nextToken
			elseif scanner.Cmode then
				syntax_error (<<scanner.type_name_token, scanner.right_square_bracket_token, scanner.this_token>>)
			else
				syntax_error (<<scanner.type_name_token, scanner.right_curly_bracket_token, scanner.this_token>>)
			end -- if
		end -- inspect
	end -- parseMemberVisibility
	
	parseMemberSelection(unitDsc: UnitDeclarationDescriptor) is 
	--72
	-- select SelectionDescriptor {“,” SelectionDescriptor}	
	--	memberSelections: Sorted_Array [SelectionDescriptor]
	-- SelectionDescriptor => Identifier[Signature]
	require
		valid_token: validToken (<<scanner.select_token>>)
		unit_descriptor_not_void: unitDsc /= Void
	local
		sDsc: SelectionDescriptor
		signatureDsc: SignatureDescriptor
		memberName: String
		toLeave: Boolean
		commaFound: Boolean
	do
		from
			scanner.nextToken
		until
			toLeave
		loop
			inspect	
				scanner.token
			when scanner.identifier_token then
				commaFound := False
				memberName := scanner.tokenString
				scanner.nextToken
				inspect
					scanner.token
				when scanner.left_paranthesis_token, scanner.colon_token, scanner.implies_token then
					signatureDsc := parseSignature
					if signatureDsc = Void then
						toLeave:= True
					end -- if
				else
					signatureDsc := Void
				end -- inspect
				if not toLeave then
					create sDsc.init (memberName, signatureDsc)
					if not unitDsc.memberSelections.added (sDsc) then
						validity_error( "Duplicated selection of `" + sDsc.out + "` in unit `" + unitDsc.name + "`")
					end -- if
				end -- if
			when scanner.comma_token then
				if commaFound then
					syntax_error (<<scanner.identifier_token>>)
					toLeave:= True
				else
					commaFound := True
				end -- if
				scanner.nextToken
			else
				if commaFound then
					syntax_error (<<scanner.identifier_token>>)
				end -- if
				toLeave:= True
			end -- inspect	
		end -- loop
	end -- parseMemberSelection

	parseInheritedOverrideTail (uDsc: UnitTypeNameDescriptor; unitDsc: UnitDeclarationDescriptor) is
	-- override uDsc.
	--              ^
	require
		utnd_not_void: uDsc /= Void
		unit_dsc_not_void: unitDsc /= Void
		valid_token: validToken (<<scanner.dot_token>>)
	local
		signDsc: SignatureDescriptor
		imoDsc : InheritedMemberOverridingDescriptor
		utnDsc: UnitTypeNameDescriptor
		ident: String
		commaFound: Boolean
		toLeave: Boolean
	do
		if unitDsc.findParent (uDsc) = Void then
			validity_error( "Override refers to the unit `" + uDsc.out + "` which is not a parent of unit `" + unitDsc.name + "`")
		end -- if
		scanner.nextToken
		if scanner.token = scanner.identifier_token then
			-- override uDsc.ident
			ident := scanner.tokenString
			scanner.nextToken
			inspect
				scanner.token
			when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
				signDsc := parseSignature
			else
			end -- inspect
			create imoDsc.init (uDsc, ident, signDsc)
			-- inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]				
			if not unitDsc.inheritedOverrides.added (imoDsc) then
				validity_error( "Duplicated overriding of `" + imoDsc.out + "` in unit `" + unitDsc.name + "`")
			end -- if
			-- Now check for the list 
			if scanner.token = scanner.comma_token then
				from
					commaFound:= True
				until
					toLeave
				loop
					inspect	
						scanner.token
					when scanner.comma_token then
						if commaFound then
							if unitDsc.inheritedOverrides.count > 1 then
								syntax_error (<<scanner.identifier_token>>)
								toLeave := True
							end -- if
						else
							commaFound := True
						end -- if
						scanner.nextToken
					when scanner.type_name_token then
						if commaFound then 
							-- UnitTypeName”.”Identifier[Signature]
							commaFound := False
							if unitDsc.hasFormalGenericParameter (scanner.tokenString) then
								-- attempt to override a member of generic parameter
								validity_error(
									"Member of the generic parameter `" + scanner.tokenString + "` can not be overrided. Only unit type members can be overrided"
								)
								scanner.nextToken
								toLeave := True
							else
								utnDsc ?= parseUnitTypeName
								if utnDsc = Void then
									toLeave := True
								else
									if scanner.token = scanner.dot_token then
										scanner.nextToken
										if scanner.token = scanner.identifier_token then
											ident := scanner.tokenString
											scanner.nextToken
											inspect
												scanner.token
											when scanner.colon_token, scanner.implies_token, scanner.left_paranthesis_token then
												signDsc := parseSignature
											else
											end -- inspect
											create imoDsc.init (utnDsc, ident, signDsc)
											-- inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]				
											if not unitDsc.inheritedOverrides.added (imoDsc) then
												validity_error( "Duplicated overriding of `" + imoDsc.out + "` in unit `" + unitDsc.name + "`")
											end -- if
										else
											syntax_error (<<scanner.identifier_token>>)
											toLeave := True
										end -- if
									else
										syntax_error (<<scanner.dot_token>>)
										toLeave := True
									end -- if
								end -- if
							end -- if
						else
							--syntax_error (<<scanner.comma_token>>)
							toLeave := True
						end -- if
					else
						toLeave := True
					end -- inspect
				end -- loop
			end -- if
		else
			syntax_error (<<scanner.identifier_token>>)
		end -- if
	end -- parseInheritedOverrideTail
	
	parseInheritedMemberOverridingOrMemberDeclaration 
		(currentVisibilityZone: MemberVisibilityDescriptor; unitDsc: UnitDeclarationDescriptor): Boolean is 
		--74
		-- override InheritedMemberOverridingDescriptor {“,” InheritedMemberOverridingDescriptor}
		-- parse "override InheritedMemberOverriding"
		--                 ^UnitTypeNameDescriptor”.”Identifier[SignatureDescriptor]
		--       "override MemberDeclaration (goToMembers := True)"	
		--                 ^[final] UnitAttribiteDeclaration|UnitRoutineDeclaration	
	require
		valid_token: validToken (<<scanner.override_token>>)
		unit_descriptor_not_void: unitDsc /= Void
	local
		utnDsc: UnitTypeNameDescriptor
		ident: String
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.final_token, scanner.pure_token, scanner.safe_token then
			-- override final UnitAttribiteDeclaration|UnitRoutineDeclaration	
			parseMember (currentVisibilityZone, unitDsc, True, Void)
			Result := True
		when scanner.const_token, scanner.rigid_token then
			-- override const UnitAttribiteDeclaration
			parseMember (currentVisibilityZone, unitDsc, True, Void)
			Result := True
		when 
			scanner.operator_token,
			scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token,
			scanner.assignment_token, scanner.left_paranthesis_token
		then
			-- override operator ...
			parseMember (currentVisibilityZone, unitDsc, True, Void)
			Result := True
		when scanner.type_name_token then
			ident := scanner.tokenString
			scanner.nextToken
			if unitDsc.hasFormalGenericParameter (ident) then
				-- attempt to override a member of generic parameter
				validity_error( "Member of the generic parameter `" + ident + "` can not be overrided. Only unit type members can be overrided")
			else
				inspect
					scanner.token
				when scanner.dot_token then
					-- parse "override UnitTypeNameDescriptor”.”Identifier[SignatureDescriptor]"
					create utnDsc.init (ident, Void)
					utnDsc ?= register_type (utnDsc)
					check
						unit_type_registered: utnDsc /= Void
					end -- check
					parseInheritedOverrideTail (utnDsc, unitDsc)
				else
					if scanner.genericsStart then
						-- parse "override identifier [UnitTypeNameDescriptor”.”Identifier[SignatureDescriptor] {", UnitTypeNameDescriptor”.”Identifier[SignatureDescriptor]"}"
						--                            ^
						utnDsc ?= parseUnitTypeName1 (ident, False)
						if utnDsc /= Void then
							-- override UnitTypeNameDescriptor.
							if scanner.token = scanner.dot_token then
								parseInheritedOverrideTail (utnDsc, unitDsc)
							else
								syntax_error (<<scanner.dot_token>>)
							end -- if
						end -- if
					elseif scanner.Cmode then
						syntax_error (<<scanner.dot_token, scanner.greater_token>>)
					else
						syntax_error (<<scanner.dot_token, scanner.right_square_bracket_token>>)
					end -- if
				end -- inspect
			end -- if
		when scanner.identifier_token then
			ident := scanner.tokenString
			scanner.nextToken
			parseMember (currentVisibilityZone, unitDsc, True, ident)
			Result := True
		--when scanner.left_paranthesis_token then
		--	scanner.nextToken
		--	if scanner.token = scanner.right_paranthesis_token then
		--		scanner.nextToken
		--		-- parse MemberDeclaration
		--		parseMember (currentVisibilityZone, unitDsc, False, "()")
		--	else
		--		syntax_error (<<scanner.right_paranthesis_token>>)
		--	end -- if
		else
			syntax_error (<<
				scanner.type_name_token, scanner.identifier_token,
				scanner.final_token, scanner.pure_token, scanner.safe_token,
				scanner.const_token, scanner.rigid_token
			>>)
		end -- inspect
	end -- parseInheritedMemberOverridingOrMemberDeclaration

	parseInitDeclaration (unitDsc: UnitDeclarationDescriptor; currentVisibilityZone: MemberVisibilityDescriptor): InitDeclarationDescriptor is 
	--75 InitDeclaration: UnitName [Parameters] [EnclosedUseDirective] [RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | (foreign [EnsureBlock end] )
	--                    ^     
	require
		non_void_current_unit: unitDsc /= Void
		valid_token: validToken(<<scanner.type_name_token>>)
	--require
	--	valid_token: validToken (<<scanner.left_paranthesis_token, scanner.use_token, scanner.require_token, scanner.foreign_token,
	--		scanner.do_token, scanner.left_curly_bracket_token>>)
	local
		parameters: Array [ParameterDescriptor]
		preconditions: Array [PredicateDescriptor]
		postconditions: Array [PredicateDescriptor]
		innerBlock: InnerBlockDescriptor
		ucb: UseConstBlock
		checkForEnd: Boolean
		isForeign: Boolean
		wasError: Boolean
	do
		scanner.nextToken
		if scanner.token = scanner.left_paranthesis_token then
			-- scanner.nextToken
			parameters := parseParameters (False)
		end -- if
		if scanner.token = scanner.use_token then
			ucb := parseEnclosedUseDirective
		end -- if
		if scanner.token = scanner.require_token then
			scanner.nextToken
			preconditions := parsePredicates
		end -- if
		inspect
			scanner.token
		when scanner.foreign_token then
			isForeign := True
			scanner.nextToken
		when scanner.none_token then
			-- innerBlock = Void
			scanner.nextToken
		else
			if scanner.blockStart then
				innerBlock := parseInnerBlock (False)
				checkForEnd := True
			elseif scanner.Cmode then
				syntax_error (<<scanner.left_curly_bracket_token, scanner.foreign_token>>)
				wasError := True
			else
				syntax_error (<<scanner.do_token, scanner.foreign_token>>)
				wasError := True
			end -- if
		end -- inspect
		if not wasError then
			if scanner.token = scanner.ensure_token then
				checkForEnd := True
				scanner.nextToken
				postconditions := parsePredicates
			end -- if
			if checkForEnd then 
				if scanner.blockEnd then
					scanner.nextToken
					if ucb = Void then
						create Result.init (unitDsc, currentVisibilityZone, parameters, Void, Void, preconditions, isForeign, innerBlock, postconditions )
					else
						create Result.init (unitDsc, currentVisibilityZone, parameters, ucb.usage, ucb.constants, preconditions, isForeign, innerBlock, postconditions )
					end -- if
				elseif scanner.Cmode then
					syntax_error (<<scanner.right_curly_bracket_token>>)
				else
					syntax_error (<<scanner.end_routine_expected>>)
				end -- if
			end -- if
		end -- if	
	end -- parseInitDeclaration
	
	parseInitProcedureInheritanceOrMemberDeclaration
		(currentVisibilityZone: MemberVisibilityDescriptor; unitDsc: UnitDeclarationDescriptor): Boolean is 
		--76
		-- parse
		-- "new	InitProcedureInheritance"
		-- new InitFromParentDescriptor {[“,”] InitFromParentDescriptor}
		--		inhertitedInits: Sorted_Array [InitFromParentDescriptor]
		-- InitFromParentDescriptor => UnitTypeName [Signature]
		-- identifier

--		-- or
--		-- "MemberDeclaration (goToMembers := True)"
--		-- new ( require do foreign use
	require
		valid_token: validToken (<<scanner.new_token>>)
		unit_descriptor_not_void: unitDsc /= Void
	local
		nmdTypeDsc: NamedTypeDescriptor
		utnDsc: UnitTypeNameDescriptor
		utnDsc1: UnitTypeNameDescriptor
		ifpDsc: InitFromParentDescriptor
		commaFound: Boolean
		toLeave: Boolean
	do
		scanner.nextToken
		inspect
			scanner.token
		when scanner.type_name_token then
			-- parse init inheritance
			from
			until
				toLeave
			loop
				inspect	
					scanner.token
				when scanner.type_name_token then
					if commaFound or else unitDsc.inhertitedInits.count = 0 then
						commaFound := False
						nmdTypeDsc := parseUnitTypeName
						if nmdTypeDsc = Void then
							toLeave := True
						else
							utnDsc ?= nmdTypeDsc
							if utnDsc = Void then
								toLeave := True
								validity_error( "Initialization procedure can not be inherited from the generic type `" + nmdTypeDsc.name + "`")
							else
								utnDsc1 := unitDsc.findParent (utnDsc)
								if utnDsc1 = Void then
									validity_error( "Initialization procedure can not be inherited from the unit `" +  utnDsc.name + "` as it is not a parent of `" + unitDsc.name + "`")
								else
									utnDsc := utnDsc1
								end -- if
debug
	--trace ("new " + utnDsc.out + " ?")
end -- debug								
								inspect	
									scanner.token
								-- when scanner.colon_token, scanner.left_paranthesis_token then
								when scanner.left_paranthesis_token then
									create ifpDsc.init (utnDsc, parseSignature)
								else
									create ifpDsc.init (utnDsc, Void)
								end -- if
								if theSameUnit1 (unitDsc, utnDsc) then
									validity_error( "Initialization procedure can not be inherited from the same unit `" + unitDsc.name + "`")
								elseif not unitDsc.inhertitedInits.added (ifpDsc) then
									validity_error( "Duplicated initialization procedure inheritance of `" + ifpDsc.out + "` in unit `" + unitDsc.name + "`")
								end -- if
							end -- if
						end -- if
					else
						toLeave := True
					end -- if					
				when scanner.comma_token then
					if commaFound then
						syntax_error (<<scanner.type_name_token>>)
						toLeave:= True
					else
						commaFound := True
					end -- if
					scanner.nextToken
				else
					if commaFound then
						syntax_error (<<scanner.type_name_token>>)
					end -- if
					toLeave:= True
				end -- inspect	
			end -- loop
		else
			syntax_error (<<scanner.type_name_token>>)
		end -- inspect
	end -- parseInitProcedureInheritanceOrMemberDeclaration
	
	parseConstObjectsDeclaration(unitDsc: UnitDeclarationDescriptor) is 
		--77
		-- ConstObjectsDeclaration: const: [ ConstObject { “,” ConstObject} ] end
		-- ConstObject : Constant | (“{” RegularExpression “}” IntegerConstant [“+”])  | (Idenitifer [ CallChain ]) [ “..”  Constant | (Idenitifer [ CallChain ]) ]
		-- RegularExpression: Constant ({“|”Constant}) | (“|” ”..” Constant)

	require
		valid_token: validToken (<<scanner.colon_token>>)
		unit_descriptor_not_void: unitDsc /= Void
	local
		cobjDsc: ConstObjectDescriptor
		toLeave: Boolean
	do
		from
			scanner.nextToken
		until
			toLeave
		loop
			cobjDsc := parseConstantObject
			if cobjDsc = Void then
				toLeave := True
			else
				if not unitDsc.constObjects.added(cobjDsc) then
					validity_error( "Duplicated constant declaration `" + cobjDsc.out + "` in unit `" + unitDsc.name  +"'") 
				end -- if
				if scanner.token = scanner.comma_token then
					scanner.nextToken
				else
					toLeave := True
				end -- if
			end -- if
		end -- loop		
		if scanner.blockEnd then
			scanner.nextToken
		elseif scanner.Cmode then
			syntax_error (<<scanner.right_curly_bracket_token>>)
		else
			syntax_error (<<scanner.end_token>>)
		end -- if
	end -- parseConstObjectsDeclaration

	parseConstantObject: ConstObjectDescriptor is
	-- ConstObject : ( Constant | (Idenitifer [ Arguments ]) [ “..”  Constant | (Idenitifer [ Arguments ]) ] ) | (“{” RegularExpression “}” IntegerConstant [“+”])
	-- RegularExpression: Constant ({“|”Constant}) | (“|” ”..” Constant)
	local
		constDsc: ConstantDescriptor
		regExpDsc: RegExpConstObjectDescriptor
		cwiDsc: ConstWithInitDescriptor
		identDsc: IdentifierDescriptor
		arguments: Array [ExpressionDescriptor]
		name: String
		operator: String
		exprDsc: ExpressionDescriptor	
		rangeRequired: Boolean
		wasError: Boolean
	do
-- not_implemented_yet
--??? все неправильно см диаграммы ....
		inspect
			scanner.token
		when scanner.identifier_token then
			name := scanner.tokenString
			scanner.nextToken
			inspect
				scanner.token
			when scanner.left_paranthesis_token then
				-- name (expr) - init short form
				create {ConstWithInitDescriptor} Result.init (name, parseArguments)
			when scanner.left_curly_bracket_token then 
				-- Iterator for range
				scanner.nextToken
				inspect
					scanner.token
				when scanner.identifier_token, scanner.operator_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
				then
					operator := scanner.tokenString
					scanner.nextToken
					exprDsc := parseExpression
					if exprDsc /= Void then
						if scanner.token = scanner.right_curly_bracket_token then
							scanner.nextToken
							create {IdentifierDescriptor} identDsc.init (name)
							create {ConstWithIteratorDescriptor} Result.init (identDsc, operator, exprDsc)
							--create {ConstWithIteratorDescriptor} Result.init (name, operator, exprDsc)
							rangeRequired := True
						else
							syntax_error (<<scanner.right_curly_bracket_token>>)
							wasError := True
						end -- if
					end -- if
				else
					syntax_error (<<
						scanner.identifier_token, scanner.operator_token, scanner.implies_token, scanner.less_token,
						scanner.greater_token, scanner.bar_token, scanner.tilda_token
					>>)
					wasError := True
				end -- inspect
			else
				-- Just identifier
				create {IdentifierDescriptor} Result.init (name)
			end -- if
			if not wasError then
				if scanner.token = scanner.period_token then
					scanner.nextToken
					inspect
						scanner.token
					when scanner.identifier_token then
						-- Init call
						create {IdentifierDescriptor} identDsc.init (scanner.tokenString)
						name := scanner.tokenString
						scanner.nextToken
						arguments := parseArguments
						if arguments = Void then
							create {ConstRangeObjectDescriptor} Result.init (Result, identDsc)
						else
							create {ConstWithInitDescriptor} cwiDsc.init (name, arguments)
							create {ConstRangeObjectDescriptor} Result.init (Result, cwiDsc)
						end -- if
					when scanner.string_const_token, scanner.char_const_token, scanner.integer_const_token, scanner.real_const_token then
						constDsc := parseConstant (False)
						if constDsc = Void then
							Result := Void
						else
							create {ConstRangeObjectDescriptor} Result.init (Result, constDsc)
						end -- if
					else
						syntax_error (<<
							scanner.identifier_token, scanner.string_const_token, scanner.char_const_token,
							scanner.integer_const_token, scanner.real_const_token
						>>)
						wasError:= True 
					end -- inspect
				elseif rangeRequired then 
					syntax_error (<<scanner.period_token>>)
					wasError:= True 
				end -- if
			end -- if
		when scanner.string_const_token, scanner.char_const_token, scanner.integer_const_token, scanner.real_const_token then
			constDsc := parseConstant (False)
			Result := constDsc
			if Result /= Void then
				if scanner.token = scanner.left_curly_bracket_token then 
					-- Iterator for range
					scanner.nextToken
					inspect
						scanner.token
					when scanner.identifier_token, scanner.operator_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token
					then
						operator := scanner.tokenString
						scanner.nextToken
						exprDsc := parseExpression
						if exprDsc /= Void then
							if scanner.token = scanner.right_curly_bracket_token then
								scanner.nextToken
								create {ConstWithIteratorDescriptor} Result.init (constDsc, operator, exprDsc)
								rangeRequired := True
							else
								syntax_error (<<scanner.right_curly_bracket_token>>)
								wasError := True
							end -- if
						end -- if
					else
						syntax_error (<<scanner.identifier_token>>)
						wasError := True
					end -- inspect
				end -- if
				if not wasError then
					if scanner.token = scanner.period_token then
						scanner.nextToken
						inspect
							scanner.token
						when scanner.identifier_token then
							-- Init call
							name := scanner.tokenString
							scanner.nextToken
							create {ConstWithInitDescriptor} cwiDsc.init (name, parseArguments)
							create {ConstRangeObjectDescriptor} Result.init (Result, cwiDsc)
						when scanner.string_const_token, scanner.char_const_token, scanner.integer_const_token, scanner.real_const_token then
							constDsc := parseConstant (False)
							if constDsc = Void then
								Result := Void
							else
								create {ConstRangeObjectDescriptor} Result.init (Result, constDsc)
							end -- if
						else
							syntax_error (<<
								scanner.identifier_token, scanner.string_const_token, scanner.char_const_token,
								scanner.integer_const_token, scanner.real_const_token
							>>)
							wasError:= True 
						end -- inspect
					elseif rangeRequired then 
						syntax_error (<<scanner.period_token>>)
						wasError:= True 
					end -- if
				end -- if
			end -- if
		when scanner.left_curly_bracket_token then
			-- Regular expression:
			-- “{” RegularExpression “}” IntegerConstant [“+”]
			-- RegularExpression:
			-- Constant {“|”Constant}
			-- |
			-- Constant “|” ”..” Constant
			regExpDsc:=  regExpDsc
not_implemented_yet ("parse regular expression in constant object declaration")
		else
			-- It is not a constant
		end -- inspect
	end -- parseConstantObject

	checkForInit (unitDsc: UnitDeclarationDescriptor; rtnDsc: UnitRoutineDeclarationDescriptor): UnitRoutineDescriptor is
	require
		current_unit_not_void: unitDsc /= Void
		current_routine_not_void: rtnDsc /= Void
	local
		wasError: Boolean
	do
		Result := rtnDsc
		if unitDsc.name.is_equal (rtnDsc.name) then
			-- That should be a vlaid init procedure !!!
			-- 
--trace ("Init found for unit " + unitDsc.name)
			if rtnDsc.type /= Void then
				-- init must be a procedure !!!
				validity_error ("Initializer of unit `" + unitDsc.name + "`" + " should not return a value, procedure expected")
				wasError := True
			end -- if
			if rtnDsc.isVirtual then
				-- init must be effective !!!
				validity_error ("Initializer of unit `" + unitDsc.name + "`" + " should not be virtual, do-foreign-none expected")
				wasError := True
			end -- if
			if rtnDsc.isOverriding then
				-- there is no overrding for init !!!
				validity_error ("Initializer of unit `" + unitDsc.name + "`" + " should not be an overiding")
				wasError := True
			end -- if
			if rtnDsc.isFinal then
				-- there is no overrding control for init !!!
				validity_error ("Initializer of unit `" + unitDsc.name + "`" + " should not be marked final")
				wasError := True
			end -- if			
			if not wasError then
				create {InitDeclarationDescriptor}Result.init (unitDsc,
					rtnDsc.visibility,
					rtnDsc.parameters,
					rtnDsc.usage,
					rtnDsc.constants,
					rtnDsc.preconditions,
					rtnDsc.isforeign,
					rtnDsc.innerblock,
					rtnDsc.postconditions
				)
--			create Result.init (
--	currentVisibilityZone,
--	parameters,
--	ucb.usage,
--	ucb.constants,
--	preconditions,
--	isForeign,
--	innerBlock,
--	postconditions
--	)

			end -- if
		end -- if
	end -- checkForInit
	
	parseUnitRoutineOrAttribute (unitDsc: UnitDeclarationDescriptor; isOverriding, isFinal: Boolean): Sorted_Array [MemberDeclarationDescriptor] is
	--78
	-- UnitRoutineDeclaration: Identifier [final Identifier] [Parameters] [“:” Type] [EnclosedUseDirective] 
	-- 		[RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | (virtual|foreign|none (“=>”Expression ) [EnsureBlock end] )
	-- or
	-- UnitAttributeDeclaration: UnitAttributeNamesList “:” Type [ (rtn “:=” [[ Parameters] HyperBlock ])|( is ConstantExpression) ]
	require
		valid_token: validToken (<<scanner.identifier_token, scanner.const_token, scanner.rigid_token>>)
	local
		name: String
		memDsc: MemberDeclarationDescriptor
		rtnDsc: UnitRoutineDeclarationDescriptor
		attrDsc: UnitAttrDescriptor
		dtDsc: DetachableTypeDescriptor
		typeDsc: TypeDescriptor
		constExprDsc: ConstExpressionDescriptor
		exprDsc: ExpressionDescriptor
	do
		inspect
			scanner.token
		when scanner.const_token, scanner.rigid_token then
			-- parse const attribute(s)
			Result := parseConstUnitAttributes (isOverriding, isFinal)
		else
			name := scanner.tokenString
			scanner.nextToken
			inspect
				scanner.token
			when scanner.is_token then
				-- ident is Expr
				--       ^    attribute with initialization
				scanner.nextToken
				constExprDsc := parseConstExpression (False)
--				exprDsc := parseExpressionWithSemicolon -- ConstantExpression !!!!
				if constExprDsc /= Void then -- Identifier is ConstantExpression
					create {AttachedUnitAttributeDeclarationDescriptor} attrDsc.init (isOverriding, isFinal, False, False, name, Void, Void, constExprDsc)
					create Result.fill (<<attrDsc>>)
				end -- if
			when scanner.implies_token then
				-- ident ->
				scanner.nextToken
--trace ("parse function")
				typeDsc := parseTypeDescriptor
				if typeDsc /= Void then
					-- UnitRoutineDeclaration: Identifier “->” Type [EnclosedUseDirective] 
					--                                             ^
					-- 		[RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | (virtual|foreign| (“=>”Expression ) [EnsureBlock end] )
					-- or
					-- UnitAttributeDeclaration: Identifier “:” Type [ (rtn “:=” [[ Parameters] HyperBlock ])|( is ConstantExpression) ]
					--                                               ^
					inspect
						scanner.token
					when scanner.require_token then 
						-- Function or it is the last attribute with no assigner in the unit and require is invariant !!!!
						memDsc := parseUnitFunctionWithNoParametersOrLastAttributeAndInvariant (unitDsc, isOverriding, isFinal, False, False, name, typeDsc)
						if memDsc /= Void then
							rtnDsc ?= memDsc 
							if rtnDsc /= Void then
								create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
							else
								create Result.fill (<<memDsc>>)
							end -- if
						end -- if
					when scanner.use_token, scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token then 
						-- function
						rtnDsc := parseUnitFunctionWithNoParameters (isOverriding, isFinal, False, False, name, typeDsc, scanner.token = scanner.one_line_function_token)
						if rtnDsc /= Void then
							create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
						end -- if
					else
						if scanner.blockStart then
							-- function
							rtnDsc := parseUnitFunctionWithNoParameters (isOverriding, isFinal, False, False, name, typeDsc, scanner.token = scanner.one_line_function_token)
							if rtnDsc /= Void then
								create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
							end -- if
						elseif scanner.Cmode then
							syntax_error (<<
								scanner.require_token, 
								scanner.use_token, scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token,
								scanner.left_curly_bracket_token
							>>)
						else
							syntax_error (<<
								scanner.require_token, 
								scanner.use_token, scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token,
								scanner.do_token
							>>)
						end --if
					end -- inspect
				end -- if
			when scanner.colon_token then
				-- ident :     parse more!!! Attribute or function!!!!
				scanner.nextToken
--trace ("parse type of attr or function")
				typeDsc := parseTypeDescriptor
				if typeDsc /= Void then
					-- UnitRoutineDeclaration: Identifier “:” Type [EnclosedUseDirective] 
					--                                             ^
					-- 		[RequireBlock] ( ( InnerBlock [EnsureBlock] end ) | (virtual|foreign|none (“=>”Expression ) [EnsureBlock end] )
					-- or
					-- UnitAttributeDeclaration: Identifier “:” Type [ (rtn “:=” [[ Parameters] HyperBlock ])|( is ConstantExpression) ]
					--                                               ^
					inspect
						scanner.token
					when scanner.rtn_token then
						-- attribute with assigner
						scanner.nextToken
						if scanner.token = scanner.assignment_token then
							scanner.nextToken
							attrDsc := parseUnitAttributAssigner (isOverriding, isFinal, name, typeDsc)
							if attrDsc /= Void then
								create Result.fill (<<attrDsc>>)
							end -- if
						else
							syntax_error (<<scanner.assignment_token>>)
						end -- if
					when scanner.is_token then
						-- attribute with initialization
						scanner.nextToken
						exprDsc := parseExpressionWithSemicolon
						if exprDsc /= Void then
							create {AttachedUnitAttributeDeclarationDescriptor} attrDsc.init (isOverriding, isFinal, True, False, name, Void, Void, exprDsc)
							create Result.fill (<<attrDsc>>)
						end -- if
					when scanner.require_token then 
						-- Function or it is the last attribute with no assigner in the unit and require is invariant !!!!
						memDsc := parseUnitFunctionWithNoParametersOrLastAttributeAndInvariant (unitDsc, isOverriding, isFinal, False, False, name, typeDsc)
						if memDsc /= Void then
							rtnDsc ?= memDsc 
							if rtnDsc /= Void then
								create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
							else
								create Result.fill (<<memDsc>>)
							end -- if
						end -- if
					when scanner.use_token, scanner.virtual_token, scanner.foreign_token, scanner.none_token, scanner.one_line_function_token then 
						-- function
						rtnDsc := parseUnitFunctionWithNoParameters (isOverriding, isFinal, False, False, name, typeDsc, scanner.token = scanner.one_line_function_token)
						if rtnDsc /= Void then
							create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
						end -- if
					else
						if scanner.blockStart then
							-- function
							rtnDsc := parseUnitFunctionWithNoParameters (isOverriding, isFinal, False, False, name, typeDsc, scanner.token = scanner.one_line_function_token)
							if rtnDsc /= Void then
								create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
							end -- if
						else
							-- attribute without assigner
							dtDsc ?= typeDsc
							if dtDsc = Void then
								create {AttachedUnitAttributeDeclarationDescriptor} attrDsc.init (isOverriding, isFinal, False, False, name, typeDsc, Void, Void)
							else
								create {DetachedUnitAttributeDeclarationDescriptor} attrDsc.init (isOverriding, isFinal, name, dtDsc.type, Void)
							end -- if
							create Result.fill (<<attrDsc>>)
						end -- if
					end -- inspect
				end -- if
			when scanner.comma_token then
				-- ident ,     that is an attribute ...
				scanner.nextToken
				create {TemporaryUnitAttributeDescriptor} attrDsc.init (False, False, name)
				create Result.fill (<<attrDsc>>)
				parseMultiAttributesDeclaration (isOverriding, isFinal, Result)
			when scanner.final_token, scanner.left_paranthesis_token, scanner.use_token, scanner.require_token,
				scanner.virtual_token, scanner.foreign_token, scanner.none_token, scanner.one_line_function_token
			then
				-- That is a routine start!
				rtnDsc := parseUnitRoutine (isOverriding, isFinal, False, False, name, Void, scanner.token = scanner.one_line_function_token)
				if rtnDsc /= Void then
					create Result.fill (<<checkForInit (unitDsc, rtnDsc)>>)
				end -- if
			else
				if scanner.blockStart then
					-- That is a routine start!
					rtnDsc := parseUnitRoutine (isOverriding, isFinal, False, False, name, Void, scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if
				elseif scanner.Cmode then
					syntax_error (<<
						scanner.final_token, scanner.left_paranthesis_token, scanner.use_token,
						scanner.require_token,
						scanner.left_curly_bracket_token,
						scanner.virtual_token, scanner.foreign_token, scanner.none_token, scanner.one_line_function_token,
						scanner.colon_token, scanner.comma_token, scanner.is_token
					>>)
				else
					syntax_error (<<
						scanner.final_token, scanner.left_paranthesis_token, scanner.use_token,
						scanner.require_token,
						scanner.do_token,
						scanner.virtual_token, scanner.foreign_token, scanner.none_token, scanner.one_line_function_token,
						scanner.colon_token, scanner.comma_token, scanner.is_token
					>>)
				end -- if
			end -- inspect		
		end -- inspect		
	end -- parseUnitRoutineOrAttribute

	parseUnitAttributAssigner (isO, isF: Boolean; name: String; typeDsc: TypeDescriptor): UnitAttrDescriptor is
	local
		assigner: AttributeAssignerDescriptor
		parameters: Array [ParameterDescriptor]
		dtDsc: DetachableTypeDescriptor
	do
		inspect
			scanner.token
		when scanner.left_paranthesis_token then
			-- Parameters HyperBlock
			parameters := parseParameters (False)
			inspect
				scanner.token
			when scanner.require_token then
				create {CustomAttributeAssignerDescriptor} assigner.init (parameters, parseHyperBlock (scanner.token))
			else
				if scanner.blockStart then
					create {CustomAttributeAssignerDescriptor} assigner.init (parameters, parseHyperBlock (scanner.token))
				elseif scanner.Cmode then
					syntax_error (<<scanner.require_token, scanner.left_curly_bracket_token>>)
				else
					syntax_error (<<scanner.require_token, scanner.do_token>>)
				end -- if
			end -- inspect
		when scanner.require_token then
			create {CustomAttributeAssignerDescriptor} assigner.init (parameters, parseHyperBlock (scanner.token))
			---- HyperBlock
			--inspect
			--	scanner.token
			--when scanner.require_token, scanner.do_token then
			--else
			--	syntax_error (<<scanner.require_token, scanner.do_token>>)
			--end -- inspect
		else
			if scanner.blockStart then
				create {CustomAttributeAssignerDescriptor} assigner.init (parameters, parseHyperBlock (scanner.token))
			else
				-- Default assigner!
				create {DefaultAttributeAssignerDescriptor} assigner
			end -- if
		end -- inspect
		dtDsc ?= typeDsc
		if dtDsc = Void then
			create {AttachedUnitAttributeDeclarationDescriptor} Result.init (isO, isF, False, False, name, typeDsc, assigner, Void)
		else
			create {DetachedUnitAttributeDeclarationDescriptor} Result.init (isO, isF, name, dtDsc.type, assigner)
		end -- if
	end -- parseUnitAttributAssigner

	parseMultiAttributesDeclaration (isO, isF: Boolean; aResult: Sorted_Array[MemberDeclarationDescriptor] ) is
	require
		array_not_void_and_has_one_element: aResult /= Void and then aResult.count = 1
	local
		attrDsc: UnitAttrDescriptor
		tmpDsc: TemporaryUnitAttributeDescriptor
		dtDsc: DetachableTypeDescriptor
		typeDsc: TypeDescriptor
		wasError: Boolean
		commaFound: Boolean
		toLeave: Boolean
		i, n: Integer
	do
		from
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.rigid_token then
				if commaFound or else aResult.count = 1 then
					commaFound := False
					scanner.nextToken
					if scanner.token = scanner.identifier_token then
						create tmpDsc.init (False, True, scanner.tokenString)
						if not aResult.added (tmpDsc) then
							setConstructionStart
							validity_error( "Duplicated attribute declaration `" + attrDsc.name + "`") 
							clearConstructionStart
							wasError := True
						end -- if
						scanner.nextToken							
					else
						syntax_error (<<scanner.identifier_token>>)
						toLeave := True
						wasError := True
					end -- if
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.const_token then 
				if commaFound or else aResult.count = 1 then
					commaFound := False
					scanner.nextToken
					if scanner.token = scanner.identifier_token then
						create tmpDsc.init (True, False, scanner.tokenString)
						if not aResult.added (tmpDsc) then
							setConstructionStart
							validity_error( "Duplicated attribute declaration `" + attrDsc.name + "`") 
							clearConstructionStart
							wasError := True
						end -- if
						scanner.nextToken							
					else
						syntax_error (<<scanner.identifier_token>>)
						toLeave := True
						wasError := True
					end -- if
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.identifier_token then
				if commaFound or else aResult.count = 1 then
					commaFound := False
					create tmpDsc.init (False, False, scanner.tokenString)
					if not aResult.added (tmpDsc) then
						setConstructionStart
						validity_error( "Duplicated attribute declaration `" + attrDsc.name + "`") 
						clearConstructionStart
						wasError := True
					end -- if
					scanner.nextToken							
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
					toLeave := True
					wasError := True
				end -- if
			when scanner.comma_token then
				if commaFound then
					syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token>>)
					toLeave := True
					wasError := True
				else
					commaFound := True
					scanner.nextToken
				end -- if
			when scanner.colon_token then
				if commaFound then
					syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token>>)
					wasError:= True
				end -- if
				toLeave := True
				scanner.nextToken
			else
				if commaFound then
					syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token, scanner.colon_token>>)
				else
					syntax_error (<<scanner.colon_token, scanner.comma_token>>)
				end -- if
				toLeave := True
				wasError := True
			end -- if
		end -- loop
		if wasError then
			aResult.make
		else
			typeDsc := parseTypeDescriptor
			if typeDsc /= Void then
				-- Set proper type for all parameters
				from 
					dtDsc ?= typeDsc
					i := 1
					n := aResult.count
				until
					i > n
				loop
					tmpDsc ?= aResult.item(i)
					check
						valid_temporary_attribute: tmpDsc /= Void
					end
					if dtDsc = Void then
						create {AttachedUnitAttributeDeclarationDescriptor} attrDsc.init (isO, isF, False, False, tmpDsc.name, typeDsc, Void, Void)
					else
						create {DetachedUnitAttributeDeclarationDescriptor} attrDsc.init (isO, isF, tmpDsc.name, dtDsc.type, Void)
					end -- if
					aResult.put (attrDsc, i)
					i := i + 1
				end -- loop
			end -- if
		end -- if
	end -- parseMultiAttributesDeclaration

	
	parseConstUnitAttributes (isOverriding, isFinal: Boolean): Sorted_Array [UnitAttrDescriptor] is
	--79 UnitAttributeDeclaration:
	-- ( const|rigid Identifier {", " [const|rigid] Identifier} “:” Type)
	-- |
	-- ( const|rigid Identifier [“:” AttachedType] is ConstantExpression) 
	
-- Redo to a different grammar pattern below!!!	

	-- 	 UnitAttributeDeclaration:	
	-- const|rigid Identifier [“:” AttachedType] is ConstantExpression  [NewLine]
	-- {“,” Identifier [“:” AttachedType] is ConstantExpression  [NewLine]}			
	require
		valid_token: validToken (<<scanner.const_token, scanner.rigid_token>>)
	local
		isConst: Boolean
		isRigid: Boolean
		constBlock: Sorted_Array [UnitAttrDescriptor]
		name: String
		attTypeDsc: AttachedTypeDescriptor
		tmpDsc: TemporaryUnitAttributeDescriptor
		exprDsc: ExpressionDescriptor
		unitAttrDsc : AttachedUnitAttributeDeclarationDescriptor
		toLeave: Boolean
		i, n: Integer
		noTailList: Boolean
		commaFound: Boolean
		wasError: Boolean
		isContinuedList: Boolean
		identExpected: Boolean
		toExit: Boolean
		srcp: expanded SourcePosition
	do
		inspect
			scanner.token
		when scanner.const_token then 
			isConst:= True
		when scanner.rigid_token then 
			isRigid:= True
		end -- inspect
		from
			create Result.make			
			identExpected := True
			scanner.nextToken
		until
			toExit
		loop
			if scanner.token = scanner.identifier_token then
				identExpected := False			
				srcp := scanner.source_position		
				name := scanner.tokenString
				create tmpDsc.init (isConst, isRigid, name)
				tmpdsc.set_rc (scanner.tokenRow, scanner.tokenCol)
				scanner.nextToken
				inspect
					scanner.token
				when scanner.comma_token then					
					if isContinuedList then
						-- No const clause list in case of multi-name constants
						toExit := True
					else
						noTailList := True
						-- const|rigid Identifier {", " [const|rigid] Identifier} “:” Type
						--                          ^
						from
							--create Result.fill (<<tmpDsc>>)
							create constBlock.fill (<<tmpDsc>>)
							toLeave := False
						until
							toLeave
						loop
							inspect
								scanner.token
							when scanner.comma_token then
								if commaFound then
									if isContinuedList then
										syntax_error (<<scanner.identifier_token, scanner.colon_token>>)
									else
										syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token, scanner.colon_token>>)
									end -- if
									wasError := True
									toLeave := True
								else
									commaFound := True
									scanner.nextToken
								end -- if				
							when scanner.colon_token then
								toLeave := True
								if commaFound then
									if isContinuedList then
										syntax_error (<<scanner.identifier_token>>)
									else
										syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token>>)
									end -- if
									wasError := True
								else
									scanner.nextToken
								end -- if
							when scanner.const_token, scanner.rigid_token, scanner.identifier_token then
								if commaFound then
									commaFound := False
									inspect
										scanner.token
									when scanner.const_token then 
										isConst := True
										isRigid := False
										scanner.nextToken
									when scanner.rigid_token then 
										isRigid := True
										isConst := False
										scanner.nextToken
									else
										isConst := False
										isRigid := False
									end -- inspect
									if scanner.token = scanner.identifier_token then
										name := scanner.tokenString
										create tmpDsc.init (isConst, isRigid, name)
										tmpdsc.set_rc (scanner.tokenRow, scanner.tokenCol)
										if constBlock.added (tmpDsc) then
											scanner.nextToken
										else
											validityError (tmpDsc.toSourcePosition, "Duplicated constant declaration `" + tmpDsc.name + "`") 
											wasError := True
										end -- if
									else
										syntax_error (<<scanner.identifier_token>>)
										wasError := True
										toLeave := True
									end -- if							
								else
									syntax_error (<<scanner.comma_token, scanner.colon_token>>)
									wasError := True
									toLeave := True
								end -- if				
							else
								if commaFound then
									if isContinuedList then
										syntax_error (<<scanner.identifier_token>>)
									else
										syntax_error (<<scanner.const_token, scanner.rigid_token, scanner.identifier_token>>)
									end -- if
								else
									syntax_error (<<scanner.comma_token, scanner.colon_token>>)
								end -- if
								wasError := True
								toLeave := True
							end -- inspect					
						end -- loop
--						if wasError then
--							Result := Void
--						else
						if not wasError then
							attTypeDsc := parseAttachedType (False)
							if attTypeDsc /= Void then
								from
									i := 1
									n := constBlock.count
								until
									i > n
								loop						
									tmpDsc ?= constBlock.item (i)
									check
										valid_type: tmpdsc /= Void
									end -- check
									create unitAttrDsc.init (isOverriding, isFinal, tmpDsc.markedConst, tmpDsc.markedRigid, tmpDsc.name, attTypeDsc, Void, Void)
									unitAttrDsc.set_rc (tmpDsc.row, tmpDsc.col)
									if not Result.added (unitAttrDsc) then
										validityError (unitAttrDsc.toSourcePosition, "Duplicated constant declaration `" + unitAttrDsc.name + "`") 
										wasError := True
									end -- if
									i := i + 1
								end -- loop
							end -- if
						end -- if
					end -- if
				when scanner.colon_token then 
					scanner.nextToken
					attTypeDsc := parseAttachedType (False)
					if attTypeDsc /= Void then
						if scanner.token = scanner.is_token then
							scanner.nextToken
							exprDsc := parseExpressionWithSemicolon
							if exprDsc /= Void then -- const|rigid Identifier “:” AttachedType is ConstantExpression
								create unitAttrDsc.init (isOverriding, isFinal, isConst, isRigid, name, attTypeDsc, Void, exprDsc)
								unitAttrDsc.setSourcePosition (srcp)
									-- init (isO, isF, mc, mr: Boolean; aName: String; aType: like type; a: like assigner; ie: like expr)
								if not Result.added (unitAttrDsc) then
									validityError (unitAttrDsc.toSourcePosition, "Duplicated constant declaration `" + unitAttrDsc.name + "`") 
									wasError := True
								end -- if
							end -- if
						else -- const|rigid Identifier “:” Type
							create unitAttrDsc.init (isOverriding, isFinal, isConst, isRigid, name, attTypeDsc, Void, exprDsc)
							unitAttrDsc.setSourcePosition (srcp)
							if not Result.added (unitAttrDsc) then
								validityError (unitAttrDsc.toSourcePosition, "Duplicated constant declaration `" + unitAttrDsc.name + "`")
								wasError := True
							end -- if
						end -- if
					end -- if
				when scanner.is_token then
					scanner.nextToken
					exprDsc := parseExpressionWithSemicolon
					if exprDsc /= Void then -- const|rigid Identifier is ConstantExpression
						create unitAttrDsc.init (isOverriding, isFinal, isConst, isRigid, name, attTypeDsc, Void, exprDsc)
						unitAttrDsc.setSourcePosition (srcp)
						if not Result.added (unitAttrDsc) then
							validityError (unitAttrDsc.toSourcePosition, "Duplicated constant declaration `" + unitAttrDsc.name + "`")
							wasError := True
						end -- if
					end -- if
				else
					syntax_error (<<scanner.comma_token, scanner.colon_token, scanner.is_token>>)
					wasError := True
				end -- inspect
				if not wasError and then not noTailList and then scanner.token = scanner.comma_token then 
--				if not wasError and then scanner.token = scanner.comma_token then 
					-- const|rigid continuation
					check
						consistent_constant: isConst or else isRigid
						--only_one_constant_found: Result.count = 1
					end -- check
					identExpected := True
					isContinuedList := True
					scanner.nextToken
-- trace ("Parse next const clause")
				else
					toExit := True
				end -- if
			else
-- trace ("Exit from const clause")
				toExit := True
				if identExpected then -- not isContinuedList then
					wasError := True
					syntax_error (<<scanner.identifier_token>>)
				end -- if
			end -- if
		end -- loop		
		if Result.count = 0 then
			Result := Void
		end -- if
	end -- parseConstUnitAttributes
	
	parseOperatorRoutine(isOverriding, isFinal, isPure, isSafe: Boolean) : UnitRoutineDescriptor is
	--80
	-- UnitRoutineDeclaration:  Operator [AliasName] [final Identifier] [Parameters] [“:” Type] [EnclosedUseDirective] ([RequireBlock] InnerBlock|virtual|foreign [EnsureBlock] [end]) | (“=>”Expression )
	require
		valid_token: validToken (<<
			scanner.operator_token, -- scanner.minus_token,
			scanner.implies_token, scanner.less_token, scanner.greater_token, 
			scanner.bar_token, scanner.tilda_token,
			scanner.assignment_token, scanner.left_paranthesis_token
		>>)
		-- Tricky:  () (Params):Type  or (): Type or () (): Type
	local
		rtnName: String
		aliasName: String
		name1: String
		wasError: Boolean
	do
--trace (">>> parseOperatorRoutine")
		if scanner.token = scanner.left_paranthesis_token then
			scanner.nextToken
			if scanner.token = scanner.right_paranthesis_token then
				scanner.nextToken
				rtnName := "()"
			else
				syntax_error (<<scanner.right_paranthesis_token>>)
				wasError:= True
			end -- if
		else
			rtnName := scanner.tokenString
			scanner.nextToken
		end -- if
		if scanner.token = scanner.alias_token then
			scanner.nextToken
			if scanner.token = scanner.identifier_token then
				aliasName := scanner.tokenString
				scanner.nextToken
				name1 := scanner.tokenString
				if aliasName.is_equal ("and") and then name1.is_equal ("then") then
					aliasName.append_character (' ')
					aliasName.append_string (name1)
					scanner.nextToken
				elseif aliasName.is_equal ("or") and then name1.is_equal ("else") then
					aliasName.append_character (' ')
					aliasName.append_string (name1)
					scanner.nextToken
				end -- if
			else
				syntax_error (<<scanner.identifier_token>>)
				wasError:= True
			end -- if
		end -- if 
		if not wasError then
--trace ("Current token " + scanner.tokenString)
			Result := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, rtnName, aliasName, False)
		end -- if
	end -- parseOperatorRoutine
	
	parseRoutineOrOneAttribute (isO, isF: Boolean; name: String): MemberDeclarationDescriptor is
	--82
	require
		member_name_not_void: name /= Void
		valid_token: validToken (<<scanner.colon_token, scanner.greater_token>>)
	local
		type: TypeDescriptor
		detachedType: DetachableTypeDescriptor
		urDsc: UnitRoutineDescriptor
		exprDsc: ExpressionDescriptor		
	do
		-- function with no parameters or attribuite
		scanner.nextToken
		type := parseTypeDescriptor	
		if type /= Void then
			inspect
				scanner.token
			when scanner.is_token then
				-- name: Type is Expr // Constant attribute!
				-- attribute with initialization
				scanner.nextToken
				exprDsc := parseExpressionWithSemicolon
				if exprDsc /= Void then
					create {AttachedUnitAttributeDeclarationDescriptor} Result.init (isO, isF, False, False, name, Void, Void, exprDsc)
				end -- if
			when scanner.rtn_token then
				-- name: Type rtn := Assigner // Attribute!
				--            ^
				scanner.nextToken
				if scanner.token = scanner.assignment_token then
					scanner.nextToken
					Result := parseUnitAttributAssigner (isO, isF, name, type)
				else
					syntax_error (<<scanner.assignment_token>>)
				end -- if
			when scanner.require_token, scanner.foreign_token, scanner.virtual_token, scanner.one_line_function_token then
				-- name: Type require | foreign | virtual | => 
				-- 												 one_line_function_token
				--            ^
				-- function with no parameters
				urDsc ?= parseAnyRoutine (isO, isF, False, False, name, Void, type, False, False, Void, scanner.token = scanner.one_line_function_token)
				Result := urDsc
			else
				if scanner.blockStart then
					-- name: Type do 
					--            ^
					-- function with no parameters
					urDsc ?= parseAnyRoutine (isO, isF, False, False, name, Void, type, False, False, Void, scanner.token = scanner.one_line_function_token)
					Result := urDsc
				else
					-- name: Type // Attribute!
					detachedType ?= type
					if detachedType = Void then
						create {AttachedUnitAttributeDeclarationDescriptor} Result.init (isO, isF,False, False, name, type, Void, Void)
					else
						create {DetachedUnitAttributeDeclarationDescriptor} Result.init (isO, isF, name, detachedType.type, Void)
					end -- if
				end -- if
			end -- inspect
		end -- if
	end -- parseRoutineOrOneAttribute

	parsePureSafe (isOverriding, isFinal, isPure, isSafe: Boolean): UnitRoutineDescriptor is
	local
		rtnName: String
	do
		inspect
			scanner.token
		when scanner.identifier_token then
			rtnName := scanner.tokenString
			scanner.nextToken		
			inspect
				scanner.token
			when scanner.final_token, scanner.left_paranthesis_token, scanner.use_token, scanner.require_token,
				scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token, scanner.colon_token
			then
				Result := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, rtnName, Void, scanner.token = scanner.one_line_function_token)
			else
				if scanner.blockStart then
					Result := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, rtnName, Void, scanner.token = scanner.one_line_function_token)
				elseif scanner.Cmode then
					syntax_error (<<scanner.final_token, scanner.left_paranthesis_token, scanner.use_token, scanner.require_token, scanner.left_curly_bracket_token,
						scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token, scanner.colon_token
					>>)
				else
					syntax_error (<<scanner.final_token, scanner.left_paranthesis_token, scanner.use_token, scanner.require_token, scanner.do_token,
						scanner.virtual_token, scanner.foreign_token, scanner.one_line_function_token, scanner.colon_token
					>>)
				end -- if
			end -- inspect
		when
			scanner.operator_token,
			--scanner.minus_token,
			scanner.implies_token,
			scanner.less_token,
			scanner.greater_token,
			scanner.bar_token,
			scanner.tilda_token,
			scanner.assignment_token,
			scanner.left_paranthesis_token
		then
			-- rotuine - operator
			Result := parseOperatorRoutine (isOverriding, isFinal, isPure, isSafe)
		else
			syntax_error (<<
				scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token,
				scanner.assignment_token, scanner.left_paranthesis_token
			>>)
		end -- if
	end -- parsePureSafe

	parseMemberDeclaration (unitDsc: UnitDeclarationDescriptor; isO: Boolean; currentVisibilityZone: MemberVisibilityDescriptor; name: String): Sorted_Array [MemberDeclarationDescriptor] is
	--83
	-- scanner.override_token, scanner.final_token, scanner.const_token, scanner.identifier_token, 
	-- scanner.operator_token, --scanner.minus_token,
	-- scanner.implies_token, scanner.bar_token, scanner.tilda_token, 
	-- pure , safe
	-- scanner.assignment_token, scanner.left_paranthesis_token
	-- name already parsed => scanner.left_paranthesis_token, scanner.colon_token, scanner.do_token, scanner.require_token
	-- MemberDeclaration: [MemberVisibility]  ([override] [final] UnitAttribiteDeclaration|UnitRoutineDeclaration) | InitDeclaration
	require
		current_unit_not_void: unitDsc /= Void
	local
		rtnDsc: UnitRoutineDescriptor
		memDsc: MemberDeclarationDescriptor
--		initDcl: InitDeclarationDescriptor
		isOverriding: Boolean
		isFinal: Boolean
		isPure: Boolean
		isSafe: Boolean
	do
		isOverriding := isO
		if name = Void then
--trace ("parseMemberDeclaration isOverriding: " + isO.out + " no name")
			inspect
				scanner.token
			when scanner.override_token then	
				-- routine or attribute
				scanner.nextToken
				isOverriding := True
				if scanner.token = scanner.final_token then
					isFinal := True
					scanner.nextToken
				end -- if
				inspect 
					scanner.token 
				when scanner.const_token, scanner.rigid_token then
					Result := parseConstUnitAttributes (isOverriding, isFinal)
-- 					--if isFinal then
					--else
					--	syntax_error (<<scanner.identifier_token, scanner.pure_token, scanner.safe_token>>)
					--end -- if
				when scanner.identifier_token then
					Result := parseUnitRoutineOrAttribute (unitDsc, isOverriding, isFinal)
				when
					scanner.operator_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token,
					scanner.assignment_token, scanner.left_paranthesis_token
				then
--trace ("operator !!! ")
					rtnDsc := parseOperatorRoutine (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if			
				when scanner.pure_token then
					-- pure routine
					scanner.nextToken		
					isPure := True
					rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if					
				when scanner.safe_token then
					-- safe routine
					scanner.nextToken		
					isSafe := True
					rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if					
				else
					syntax_error (<<
						scanner.override_token, scanner.final_token, scanner.const_token, scanner.rigid_token, scanner.left_paranthesis_token,
						scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.pure_token, scanner.safe_token, scanner.assignment_token
					>>)
				end -- inpsect
			when scanner.final_token then	
				-- routine or attribute
				isFinal := True
				scanner.nextToken		
				inspect 
					scanner.token 
				when scanner.const_token, scanner.rigid_token then
					Result := parseConstUnitAttributes (isOverriding, isFinal)
					--if isFinal then
					--else
					--	syntax_error (<<scanner.identifier_token, scanner.pure_token, scanner.safe_token>>)
					--end -- if
				when scanner.identifier_token then
					Result := parseUnitRoutineOrAttribute (unitDsc, isOverriding, isFinal)
				when
					scanner.operator_token, -- scanner.minus_token, 
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token,
					scanner.assignment_token, scanner.left_paranthesis_token
				then
					rtnDsc := parseOperatorRoutine (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if			
				when scanner.pure_token then
					-- pure routine
					scanner.nextToken		
					isPure := True
					rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if					
				when scanner.safe_token then
					-- safe routine
					scanner.nextToken		
					isSafe := True
					rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if					
				else
					syntax_error (<<
						scanner.override_token, scanner.final_token, scanner.const_token, scanner.rigid_token, scanner.left_paranthesis_token,
						scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.pure_token, scanner.safe_token, scanner.assignment_token
					>>)
				end -- inpsect
			when scanner.identifier_token, scanner.const_token, scanner.rigid_token then
				-- routine or attribute
--trace ("Unit or attribute - " + scanner.tokenString)
				Result := parseUnitRoutineOrAttribute (unitDsc, isOverriding, isFinal)
--trace ("NEXT!")
			when
				scanner.operator_token, --scanner.minus_token,
				scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.bar_token, scanner.tilda_token,
				scanner.assignment_token, scanner.left_paranthesis_token
			then
				-- rotuine - operator
				rtnDsc := parseOperatorRoutine (isOverriding, isFinal, isPure, isSafe)
				if rtnDsc /= Void then
					create Result.fill (<<rtnDsc>>)
				end -- if			
			when scanner.pure_token then
				-- pure routine
				scanner.nextToken		
				isPure := True
				rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
				if rtnDsc /= Void then
					create Result.fill (<<rtnDsc>>)
				end -- if					
			when scanner.safe_token then
				-- safe routine
				scanner.nextToken		
				isSafe := True
				rtnDsc := parsePureSafe (isOverriding, isFinal, isPure, isSafe)
				if rtnDsc /= Void then
					create Result.fill (<<rtnDsc>>)
				end -- if					
			else
				syntax_error (<<
					scanner.override_token, scanner.final_token, scanner.const_token, scanner.rigid_token,
					scanner.identifier_token, scanner.operator_token,
					scanner.implies_token,
					scanner.less_token, scanner.greater_token, scanner.pure_token, scanner.safe_token,
					scanner.assignment_token
				>>)
			end -- inpsect
		else
--trace ("parseMemberDeclaration isOverriding: " + isO.out + " name " + name)
			if scanner.Cmode and then scanner.token = scanner.left_curly_bracket_token then
				-- routine parameters
				rtnDsc := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, name, Void, scanner.token = scanner.one_line_function_token)
				if rtnDsc /= Void then
					create Result.fill (<<rtnDsc>>)
				end -- if
			else
				inspect
					scanner.token
				when scanner.alias_token, scanner.left_paranthesis_token, scanner.do_token, scanner.require_token, scanner.one_line_function_token, scanner.final_token then	
					-- routine parameters
					rtnDsc := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, name, Void, scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if
				when scanner.implies_token then
					rtnDsc := parseUnitRoutine (isOverriding, isFinal, isPure, isSafe, name, Void, scanner.token = scanner.one_line_function_token)
					if rtnDsc /= Void then
						create Result.fill (<<rtnDsc>>)
					end -- if
				when scanner.colon_token then	
					memDsc := parseRoutineOrOneAttribute (isOverriding, isFinal, name)
					if memDsc /= Void then
						create Result.fill (<<memDsc>>)
					end -- if
				else
					syntax_error (<<
						scanner.alias_token, scanner.left_paranthesis_token,
						scanner.colon_token, scanner.implies_token
						scanner.do_token, scanner.require_token,
						scanner.one_line_function_token, scanner.final_token
					>>)
				end -- inpsect
			end -- if
		end -- if
	end -- parseMemberDeclaration

	parseMember (currentVisibilityZone: MemberVisibilityDescriptor; unitDsc: UnitDeclarationDescriptor; isOverriding: Boolean; name: String) is
	--84
	local
		members: Sorted_Array [MemberDeclarationDescriptor]
		mdDsc: MemberDeclarationDescriptor
		i, n: Integer
	do
		members := parseMemberDeclaration (unitDsc, isOverriding, currentVisibilityZone, name)
		if members /= Void then
			from
				i := 1
				n := members.count
			until
				i > n
			loop
				mdDsc := members.item (i)
				--if mdDsc.visibility = Void then
				--	mdDsc.setVisibility (currentVisibilityZone)
				--end -- if
				if not unitDsc.unitMembers.added (mdDsc) then
					--validity_error( "Duplicated declaration of member `" + mdDsc.name + "` in unit `" + unitDsc.name + "`") 
					validityError (mdDsc.toSourcePosition, "Duplicated declaration of member `" + mdDsc.name + "` in unit `" + unitDsc.name + "`") 
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- parseMember

	parsePredicates: Array [PredicateDescriptor] is 
		--85
		-- PredicatesList : [Predicate {[”;”] Predicate}]
		-- Predicate      : BooleanExpression [DocumentingComment]
	local
		exprDsc: ExpressionDescriptor
		predicateDsc: PredicateDescriptor
		toLeave: Boolean	
	do
		from
			create Result.make (1, 0)
		until
			toLeave
		loop
			exprDsc := parseCommentedExpression
			if exprDsc = Void then
				toLeave := True
			else
				inspect	
					scanner.token
				when scanner.comment_token then
					create predicateDsc.init (exprDsc, scanner.tokenString)
					Result.force (predicateDsc, Result.count + 1)
					scanner.nextToken
				else
					create predicateDsc.init (exprDsc, Void)
					Result.force (predicateDsc, Result.count + 1)
				end -- inspect
			end -- if
		end -- loop
	end -- parsePredicates

	parseFactualGenerics: Array [TypeOrExpressionDescriptor] is
	require
		valid_token: validToken (<<scanner.left_square_bracket_token, scanner.less_token>>) 
	local
		typeOrExprDsc: TypeOrExpressionDescriptor
		toLeave: Boolean
		commaFound: Boolean
	do
		from
			create Result.make (1, 0)
			scanner.nextToken
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.type_name_token, scanner.identifier_token, scanner.as_token, scanner.rtn_token then
				-- Type ...
				if commaFound or else Result.count = 0 then
					commaFound := False
					typeOrExprDsc := parseFactualGenericType
					if typeOrExprDsc = Void then
						toLeave := True
						Result := Void
					else
						Result.force(typeOrExprDsc, Result.count + 1)
					end -- if
				else
					syntax_error (<<scanner.comma_token>>)
					Result := Void
					toLeave := True
				end -- if
			when scanner.comma_token then
				if commaFound or else Result.count = 0 then
					if scanner.Cmode then
						syntax_error (<<scanner.type_name_token, scanner.greater_token>>)
					else
						syntax_error (<<scanner.type_name_token, scanner.right_square_bracket_token>>)
					end -- if
					Result := Void
					toLeave := True
				else
					scanner.nextToken
					commaFound := True
				end -- if
			else
				if scanner.genericsEnd then
					if commaFound or else Result.count = 0 then
						Result := Void
						syntax_error (<<scanner.type_name_token, scanner.identifier_token>>)
					end -- if
					scanner.nextToken
					toLeave := True				
--trace ("%TparseFactualGenerics: ]")
				elseif commaFound then
					Result := Void
					syntax_error (<<scanner.type_name_token, scanner.identifier_token>>)
				elseif scanner.Cmode then
					Result := Void
					syntax_error (<<scanner.comma_token, scanner.greater_token>>)
				else
					Result := Void
					syntax_error (<<scanner.comma_token, scanner.right_square_bracket_token>>)
				end -- if
				toLeave := True
			end -- inspect
		end -- loop
	end -- parseFactualGenerics

	parseFormalGenerics (fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]): Array [FormalGenericDescriptor] is
	--	FormalGenerics: “[”FormalGeneric {“,” FormalGeneric}“]”
	--	FormalGeneric: Identifier ([“extend” UnitTypeName] [“new” [Signature]])| [“:” (UnitType | RoutineType]
	require
		valid_token: validToken (<<scanner.left_square_bracket_token, scanner.less_token>>) 
	local
		fgt: FormalGenericDescriptor
		toLeave: Boolean
		commaFound: Boolean
	do
-- trace (">>> parseFormalGenerics")
		from
			create Result.make (1, 0)
			scanner.nextToken
		until
			toLeave
		loop
			inspect
				scanner.token
			when scanner.type_name_token, scanner.identifier_token then
				-- Type ...
				if commaFound or else Result.count = 0 then
					commaFound := False
					fgt := parseFormalGenericType (fgTypes)
					if fgt = Void then
						toLeave := True
						Result := Void
					else
						Result.force(fgt, Result.count + 1)
					end -- if
				else
					syntax_error (<<scanner.comma_token>>)
					Result := Void
					toLeave := True
				end -- if
			when scanner.comma_token then
				if commaFound or else Result.count = 0 then
					if scanner.Cmode then
						syntax_error (<<scanner.type_name_token, scanner.greater_token>>)
					else
						syntax_error (<<scanner.type_name_token, scanner.right_square_bracket_token>>)
					end -- if
					Result := Void
					toLeave := True
				else
					scanner.nextToken
					commaFound := True
				end -- if
			else
				if scanner.genericsEnd then
					if commaFound or else Result.count = 0 then
						Result := Void
						syntax_error (<<scanner.type_name_token>>)
					end -- if
					scanner.nextToken
					toLeave := True				
--trace ("%TparseFormalGenerics: ]")
				elseif commaFound then
					Result := Void
					syntax_error (<<scanner.type_name_token>>)
				elseif scanner.Cmode then
					Result := Void
					syntax_error (<<scanner.comma_token, scanner.greater_token>>)
				else
					Result := Void
					syntax_error (<<scanner.comma_token, scanner.right_square_bracket_token>>)
				end -- if
				toLeave := True
			end -- inspect
		end -- loop
-- trace ("<<<parseFormalGenerics")
		if Result /= Void and then fgTypes /= Void and then fgTypes.count > 0 then
			ast.setFGpool (fgTypes)
		end -- if
	end -- 	parseFormalGenerics

	parseUnit (is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend: Boolean) is
	--87
	-- [final] [ref|val|concurrent|virtual|extend]
	-- unit Identifier 
	-- alias	[AliasName]
	-- "["		[FormalGenerics] 
	-- extend	[InheritDirective]
	-- use		[EnclosedUseDirective]
	-- select	[MemberSelection]
	-- override	[InheritedMemberOverriding]
	-- new		[InitProcedureInheritance]
	-- const	[ConstObjectsDeclaration]
	-- {
	--	"{"( MemberVisibility “:” {MemberDeclaration}) |
	--	"{"|override|final|identifier|OperatorSign MemberDeclaration    => [MemberVisibilityDescriptor]  [override] [final] UnitAttribiteDeclaration|UnitRoutineDeclaration
	--	}
	-- require	[InvariantBlock]
	-- end
	require
		valid_token: validToken (<<scanner.type_name_token, scanner.unit_token>>)
	local	
		goToMembers: Boolean
		mvDsc, currentVisibilityZone: MemberVisibilityDescriptor
		invPredicates: Array [PredicateDescriptor]
		formalGenerics: Array [FormalGenericDescriptor]
		fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]
		unitUsageAndConst: UseConstBlock
		unitName: String
		typeName: String
		initDsc: InitDeclarationDescriptor
		toLeave: Boolean
		initialErrorsCount: Integer
	do
		initialErrorsCount := errorsCount
		if scanner.token = scanner.unit_token then
			scanner.nextToken
			inspect
				scanner.token 
			when scanner.type_name_token then
				unitName:= scanner.tokenString
				scanner.nextToken
			when scanner.left_paranthesis_token then
				scanner.nextToken
				if scanner.token = scanner.right_paranthesis_token then
					scanner.nextToken
					unitName:= "()"
				else
					syntax_error (<<scanner.right_paranthesis_token>>)
				end -- if
			else
				syntax_error (<<scanner.type_name_token>>)
			end -- inspect
		else
			unitName:= scanner.tokenString
			scanner.nextToken
		end -- if
		if unitName /= Void then
			create currentUnitDsc.init (unitName, is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend)
			currentUnitDsc.attach_pools (ast)
			
			if scanner.token = scanner.alias_token then
				-- parse alias unit name
				scanner.nextToken
				inspect	
					scanner.token
				when scanner.type_name_token then -- parse alias name
					currentUnitDsc.setAliasName (scanner.tokenString)
					scanner.nextToken
				else
					syntax_error (<<scanner.type_name_token>>)
				end
			end -- if

			if scanner.genericsStart then
				-- parse formal generics
				create fgTypes.make
				formalGenerics := parseFormalGenerics (fgTypes)
				if formalGenerics /= Void then
					currentUnitDsc.setFormalGenercis (formalGenerics, fgTypes)
				end -- if
			end -- if
			o.putLine ("Parsing unit `" + currentUnitDsc.fullUnitName + "`")

			if scanner.token = scanner.extend_token then
				-- parse inheritance clause
				parseInheritanceClause (currentUnitDsc)
			end -- if

			if scanner.token = scanner.use_token then
				-- parse EnclosedUseDirective
				unitUsageAndConst := parseEnclosedUseDirective
				if unitUsageAndConst /= Void then
					currentUnitDsc.setUseConstBlock (unitUsageAndConst)
				end -- if				
			end -- if

			if scanner.token = scanner.select_token then
				-- parse "select MemberSelection"
				parseMemberSelection (currentUnitDsc)
			end -- if

			currentVisibilityZone := anyDsc

			if scanner.token = scanner.override_token then
				-- parse "override InheritedMemberOverriding" or "override MemberDeclaration (goToMembers := True)"
				goToMembers:= parseInheritedMemberOverridingOrMemberDeclaration (currentVisibilityZone, currentUnitDsc)
			end -- if

			if scanner.token = scanner.new_token and then not goToMembers then
				-- parse "new	InitProcedureInheritance" or "MemberDeclaration (goToMembers := True)"
				goToMembers:= parseInitProcedureInheritanceOrMemberDeclaration (currentVisibilityZone, currentUnitDsc)
			end -- if

			if scanner.token = scanner.const_token and then not goToMembers then
				scanner.push
				scanner.nextToken
				if scanner.token = scanner.colon_token then
					-- parse "const :	ConstObjectsDeclaration"
					scanner.flush
--trace ("Parse const objects")
					parseConstObjectsDeclaration (currentUnitDsc)
				else
					-- It is ordinary const start ...
--trace ("Parse const attribute #1")
					scanner.push
					scanner.revert
--trace ("Parse const attribute #2")
				end -- if
			end -- if

			-- parse unit members
			from
				toLeave := errorsCount > 0
			until
				toLeave
			loop
				inspect
					scanner.token
				when scanner.final_token, scanner.pure_token, scanner.safe_token,
					scanner.identifier_token, scanner.operator_token,
					scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
					scanner.bar_token,
					scanner.const_token, scanner.rigid_token, scanner.assignment_token
				then
					parseMember (currentVisibilityZone, currentUnitDsc, False, Void)
				when scanner.override_token then
					-- parse MemberDeclaration
					scanner.nextToken
					inspect
						scanner.token
					when scanner.final_token, scanner.pure_token, scanner.safe_token,
						scanner.identifier_token, scanner.operator_token,
						scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
						scanner.bar_token,
						scanner.const_token, scanner.rigid_token, scanner.assignment_token
					then
						parseMember (currentVisibilityZone, currentUnitDsc, True, Void)
					when scanner.left_paranthesis_token then
						scanner.nextToken
						if scanner.token = scanner.right_paranthesis_token then
							scanner.nextToken
							-- parse MemberDeclaration
							parseMember (currentVisibilityZone, currentUnitDsc, False, "()")
						else
							syntax_error (<<scanner.right_paranthesis_token>>)
							toLeave := True
						end -- if
					else
						if scanner.Cmode then
							syntax_error (<<
								scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
								scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token,
								scanner.tilda_token,
								scanner.bar_token,
								scanner.left_square_bracket_token,
								scanner.assignment_token
							>>)
						else
							syntax_error (<<
								scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
								scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
								scanner.implies_token, scanner.tilda_token,
								scanner.bar_token,
								scanner.left_curly_bracket_token,
								scanner.assignment_token
							>>)
						end -- if
						toLeave := True
					end -- inspect
				when scanner.type_name_token then
					typeName := scanner.tokenString
					if typeName.is_equal (unitName) then
						-- That is init start !!!
						initDsc := parseInitDeclaration (currentUnitDsc, currentVisibilityZone) 
						if initDsc /= Void then
							if not currentUnitDsc.initMembers.added (initDsc) then
								-- Duplicated init 
								validityError (initDsc.toSourcePosition, "Duplicated declaration of unit  `" + unitName + "` initializer") 
							end -- if
						end -- if
					else
						if scanner.Cmode then
							syntax_error (<<
								scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
								scanner.identifier_token, scanner.operator_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token,
								scanner.tilda_token,
								scanner.bar_token,
								scanner.left_square_bracket_token,
								scanner.assignment_token
							>>)
						else
							syntax_error (<<
								scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
								scanner.identifier_token, scanner.operator_token,
								scanner.implies_token, scanner.tilda_token,
								scanner.bar_token,
								scanner.left_curly_bracket_token,
								scanner.assignment_token
							>>)
						end -- if
						toLeave := True
					end -- if					
				when scanner.left_paranthesis_token then
					scanner.nextToken
					if scanner.token = scanner.right_paranthesis_token then
						scanner.nextToken
						-- parse MemberDeclaration
						parseMember (currentVisibilityZone, currentUnitDsc, False, "()")
					else
						syntax_error (<<scanner.right_paranthesis_token>>)
						toLeave := True
					end -- if
				when scanner.require_token then 
					toLeave := True
				else
					if scanner.blockEnd then
						toLeave := True
					elseif scanner.visibilityStart then
						--	"{" MemberVisibility “:” {MemberDeclaration}
						--	"{" MemberVisibility MemberDeclaration
						mvDsc := parseMemberVisibility (currentUnitDsc)
						if mvDsc /= Void then
							inspect
								scanner.token
							when scanner.colon_token then
								currentVisibilityZone := mvDsc
								scanner.nextToken
							when scanner.final_token, scanner.pure_token, scanner.safe_token,
								scanner.identifier_token, scanner.operator_token,
								scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
								scanner.bar_token,
								scanner.const_token, scanner.rigid_token, scanner.assignment_token
							then
								-- parse MemberDeclaration
--trace ("member with visibility " + mvDsc.out)
								parseMember (mvDsc, currentUnitDsc, False, Void)
							when scanner.type_name_token then
								typeName := scanner.tokenString
								if typeName.is_equal (unitName) then
									-- That is init start !!!
									initDsc := parseInitDeclaration (currentUnitDsc, currentVisibilityZone) 
									if initDsc /= Void then
										if not currentUnitDsc.initMembers.added (initDsc) then
											-- Duplicated init 
											validityError (initDsc.toSourcePosition, "Duplicated declaration of unit  `" + unitName + "` initializer") 
										end -- if
									end -- if
								else
									if scanner.Cmode then
										syntax_error (<<
											scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
											scanner.identifier_token, scanner.operator_token,
											scanner.implies_token, scanner.less_token, scanner.greater_token,
											scanner.tilda_token,
											scanner.bar_token,
											scanner.left_square_bracket_token,
											scanner.assignment_token
										>>)
									else
										syntax_error (<<
											scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
											scanner.identifier_token, scanner.operator_token,
											scanner.implies_token, scanner.tilda_token,
											scanner.bar_token,
											scanner.left_curly_bracket_token,
											scanner.assignment_token
										>>)
									end -- if
									toLeave := True
								end -- if							
							when scanner.override_token	then
								-- parse MemberDeclaration
								scanner.nextToken
								inspect
									scanner.token
								when scanner.final_token, scanner.pure_token, scanner.safe_token, 
									scanner.identifier_token, scanner.operator_token, 
									scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
									scanner.bar_token,
									scanner.const_token, scanner.rigid_token, scanner.assignment_token
								then
									parseMember (mvDsc, currentUnitDsc, True, Void)
								when scanner.left_paranthesis_token then
									scanner.nextToken
									if scanner.token = scanner.right_paranthesis_token then
										scanner.nextToken
										-- parse MemberDeclaration
										parseMember (currentVisibilityZone, currentUnitDsc, False, "()")
									else
										syntax_error (<<scanner.right_paranthesis_token>>)
										toLeave := True
									end -- if
								else
									if scanner.Cmode then
										syntax_error (<<
											scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
											scanner.identifier_token, scanner.operator_token, -- scanner.minus_token,
											scanner.implies_token, scanner.less_token, scanner.greater_token,
											scanner.tilda_token,
											scanner.bar_token,
											scanner.left_square_bracket_token,
											scanner.assignment_token
										>>)
									else
										syntax_error (<<
											scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
											scanner.identifier_token, scanner.operator_token, -- scanner.minus_token, 
											scanner.implies_token, scanner.tilda_token,
											scanner.bar_token,
											scanner.left_curly_bracket_token,
											scanner.assignment_token
										>>)
									end -- if
									toLeave := True
								end -- inspect
							when scanner.left_paranthesis_token then
								scanner.nextToken
								if scanner.token = scanner.right_paranthesis_token then
									scanner.nextToken
									-- parse MemberDeclaration
									parseMember (mvDsc, currentUnitDsc, False, "()")
								else
									syntax_error (<<scanner.right_paranthesis_token>>)
									toLeave := True
								end -- if
							else
								syntax_error (<<
									scanner.override_token, scanner.final_token, scanner.pure_token, scanner.safe_token, scanner.left_paranthesis_token,
									scanner.identifier_token, scanner.type_name_token, scanner.operator_token, -- scanner.minus_token,
									scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
									scanner.bar_token, scanner.left_curly_bracket_token,
									scanner.assignment_token
								>>)
								toLeave := True
							end -- inspect
						end -- if
					elseif scanner.Cmode then
						syntax_error (<<
							scanner.override_token, scanner.final_token, scanner.pure_token, scanner.safe_token,
							scanner.identifier_token, scanner.type_name_token, scanner.operator_token, -- scanner.minus_token,
							scanner.implies_token, scanner.less_token, scanner.greater_token, scanner.tilda_token,
							scanner.bar_token,
							scanner.left_square_bracket_token,
							scanner.assignment_token
						>>)
						toLeave := True
					else
						syntax_error (<<
							scanner.override_token, scanner.final_token, scanner.pure_token, scanner.safe_token,
							scanner.identifier_token, scanner.type_name_token, scanner.operator_token, -- scanner.minus_token,
							scanner.implies_token, scanner.tilda_token,
							scanner.bar_token, 
							scanner.left_curly_bracket_token,
							scanner.assignment_token
						>>)
						toLeave := True
					end -- if
				end -- inspect
			end -- loop

			if scanner.token = scanner.require_token then -- and then errorsCount = 0 then 
				-- parse unit invariant
				scanner.nextToken
				invPredicates := parsePredicates
				if invPredicates /= Void then
					currentUnitDsc.setInvariant (invPredicates)
				end -- if
			end -- if

			if scanner.blockEnd then
				-- end of the unit
				if initialErrorsCount = errorsCount and then not ast.units.added (currentUnitDsc) then
					validity_error( "More than one unit with name `" + currentUnitDsc.name + "` in the same source/compilation")
				end -- if
				scanner.nextToken
			else
				syntax_error (<<scanner.end_unit_expected>>)
			end -- if
		end -- if
		ast.setFGpool (Void)
		currentUnitDsc := Void
	end -- parseUnit

	
feature {None}

	currentUnitDsc: UnitDeclarationDescriptor -- may be Void while parsing standalone routines or anonymous one
	
	validityError (srcp: expanded SourcePosition; message: String) is
	require
		message_not_void: message /= Void		
	do
		if errorsCount = 0 then
			o.newLine
		end -- if
		errorsCount := errorsCount + 1		
		if srcp.col = 0 then
			if srcp.row > 0 then
				o.putNL ("Error at line " + srcp.row.out + " - " + message)
			else
				o.putNL ("Error - " + message)
			end -- if
		elseif srcp.row > 0 then
			o.putNL ("Error at " + srcp.row.out + ":" + srcp.col.out + " - " + message)
		else
			o.putNL ("Error - " + message)
		end -- if
	end -- validityError
	
	constructionStart: expanded SourcePosition
	
	setConstructionStart is
	do
		constructionStart := scanner.source_position
	end -- setConstructionStart
	
	clearConstructionStart is
	do
		constructionStart.set_rc(0, 0)
	end -- clearConstructionStart
	
	validity_error (message: String) is
	require
		message_not_void: message /= Void		
	do
		if errorsCount = 0 then
			o.newLine
		end -- if
		errorsCount := errorsCount + 1
		if constructionStart.isClear then
			o.putNL ("Error at line " + scanner.tokenRow.out + " - " + message)
		else
			o.putNL ("Error at " + constructionStart.row.out + ":" + constructionStart.col.out + " - " + message)
		end -- if
	end -- validity_error

	validity_warning (message: String) is
	require
		message_not_void: message /= Void
	do
		if warningsCount = 0 then
			o.newLine
		end -- if
		warningsCount := warningsCount + 1
		if constructionStart.isClear then
			o.putNL ("Warning at line " + scanner.tokenRow.out + " - " + message)
		else
			o.putNL ("Warning at " + constructionStart.row.out + ":" + constructionStart.col.out + " - " + message)
		end -- if
	end -- validity_warning
	
	syntax_error (tokens_expected: Array [Integer]) is
	do
		syntaxError (Void, tokens_expected, <<scanner.eof_token>>) -- skip till end fo file!!! Temporary !!!
	end -- syntax_error
	
	syntaxError (message: String; tokens_expected, followers: Array [Integer]) is
	require
		non_void_tokens_expected: tokens_expected /= Void
		non_void_followers: followers /= Void
	local	
		i, n: Integer
		toLeave: Boolean
		--skipTillSeparator: Boolean
	do
		if errorsCount = 0 then
			o.newLine
		end -- if
		errorsCount := errorsCount + 1
		if scanner.tokenCol = 0 then
			o.put ("Error at line " + scanner.tokenRow.out + " - ")
		else
			o.put ("Error at " + scanner.tokenRow.out + ":" + scanner.tokenCol.out + " - ")
		end -- if
		if message /= Void then
			o.put (message + ": ")
		end -- if
		o.put ("`" + scanner.tokenName(scanner.token) + "` is found, but expected: `")
		from 
			i := 1
			n := tokens_expected.count
		until
			i > n
		loop
			o.put (scanner.tokenName(tokens_expected.item (i)))			
			if i /= n then
				o.put (" ")
			end
			i := i + 1
		end
		o.put ('`')
		o.newLine
		--if True then -- toForward then
---- Temporary solution when follwers are not used!!!
--inspect 
--	tokens_expected.item(1)
--when scanner.end_if_expected, scanner.end_block_expected, scanner.end_unit_expected, scanner.end_routine_expected, scanner.end_loop_expected then
--	followers.put (scanner.end_token, 1)
--else
--end -- inspect
--followers.force (scanner.semicolon_token, followers.count + 1) // Stop skiping at the end of line or semicolon
			
			--from 
			--	n := followers.count
			--	i := 1
			--until
			--	i > n
			--loop
			--	if followers.item (i) = scanner.semicolon_token then
			--		skipTillSeparator := True
			--		i := n + 1
			--	end -- if
			--	i := i + 1
			--end -- loop
			
			from
				n := followers.count
			until
				toLeave --or else scanner.token = scanner.eof_token
			loop
				scanner.nextToken
				--scanner.nextWithSemicolon (skipTillSeparator)
				inspect 
					scanner.token
				when scanner.eof_token then
					toLeave := True
				--when scanner.end_if_expected, scanner.end_block_expected, scanner.end_unit_expected, scanner.end_routine_expected, scanner.end_loop_expected then
				--	scanner.nextToken
				--	o.putNL ("Code skipped upto " + scanner.tokenRow.out + ":" + scanner.tokenCol.out + " - `" + scanner.tokenName(scanner.token) + "`, parsing resumed")
				--	toLeave := True
				else
					from 
						i := 1
					until
						i > n
					loop
						--if followers.item (i) = scanner.semicolon_token then
						--	skipTillSeparator := True
						--end -- if
						if scanner.token = followers.item (i) then
							o.putNL ("Code skipped upto " + scanner.tokenRow.out + ":" + scanner.tokenCol.out + " - `" + scanner.tokenName(scanner.token) + "`, parsing resumed")
							toLeave := True
							i := n + 1
						else
							i := i + 1
						end -- if
					end -- loop
					--if not toLeave then
					--	inspect 
					--		scanner.token
					--	when scanner.end_if_expected, scanner.end_block_expected, scanner.end_unit_expected, scanner.end_routine_expected, scanner.end_loop_expected then
					--		--scanner.nextToken
					--		o.putNL ("Code skipped upto nearest end at " + scanner.tokenRow.out + ":" + scanner.tokenCol.out + " and parsing resumed")
					--		toLeave := True
					--	else
					--	end -- inspect
					--end -- if
				end -- inspect
			end -- loop
		--end -- if
	end -- syntaxError

	--toForward: Boolean is True

	scanner: SLang_Scanner

	init (aScanner: like scanner; sys: like systems; output: like o) is
	require
		scanner_not_void: aScanner /= Void
		scanner_ready_to_work: aScanner.isReady
		--systems_not_void: sys /= Void
		output_not_void: output /= Void
	do
		o := output
		scanner := aScanner
		create ast.init (scanner)
		scanner.setPool (ast.stringPool)
		systems := sys
		if systems = Void then
			create systems.make
		end
	end -- init

end -- class SLang_Parser