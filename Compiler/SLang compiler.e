class SLangConstants
feature
	IRfolderName: String is "_$IR"
	ASText: String is "ast"
	INText: String is "int"
	ScriptSuffix: String is "$S"
	RoutinesSuffix: String is "$R"
	UnitSuffix: String is "$U"
	SLangExt: String is "slang"
	CLangExt: String is "clang"
	SLNG_LIB: String is "SLNG_LIB"
	SLNG_BIN: String is "SLNG_BIN"
end -- class SLangConstants

class SLangCompiler
inherit
	SLangConstants
	Server
creation
	init
feature {None}
	o: Output
	systems: Sorted_Array [SystemDescriptor]
	--memory: Memory is
	--once
	--	create Result
	--end

	--delay is 
	--local
	--	i, n : Integer
	--do
	--	from
	--		i := 0
	--		n := 100000
	--	until
	--		i >= n
	--	loop
	--		i := i + 1
	--	end
	--end -- delay

	init (args: Array[String]) is 
	local
		scanner: SLang_scanner
		parser: SLang_parser
		sysDsc: SystemDescriptor
		builder: SLang_builder
		scriptsToBuild: Sorted_Array [String]
		files: Array [FSys_Dat]
		fName: String
		sName: String
		tsfName: String
		ext: String
		slangFileCount: Integer
		--actualFiles: Integer
		saveErrCount: Integer
		skipBuild: Boolean
		skipSourceFile: Boolean
		removeTimeStampFile: Boolean 
		Cmode: Boolean
		i, n: Integer
		j, m: Integer
		-- It should be commented out in the final version!
		dumpOutput: Output
	do
		create {ScreenOutput}o		
		o.putNL ("SLang compiler v0.99.14 (Build <AVK January 3rd 2023>)")
		if args = Void then
			o.putNL ("Valid usage: slc *|(<file_name1> <file_name2> ...)")
		else
			n := args.count
			if n = 1 and then args.item (1).is_equal ("*") then
				-- Let's build a list of all SLang files in the current folder
				files := fs.file_list (".")
				if files = Void then
					o.putNL ("No source files to process in the current folder...")
				else
					from
						j := 1
						m := files.count
					until 
						j > m	
					loop
						fName := files.item (j).name
						ext := fs.getFileExtension (fName)
						if ext.is_equal (SLangExt) or else ext.is_equal (CLangExt) then
							if slangFileCount = 0 then
								args.put (fName, 1)
							else	
								args.force (fName, args.count + 1)
							end
							slangFileCount := slangFileCount + 1
						end -- if
						j := j + 1
					end	-- loop
					n := slangFileCount
					files := Void
				end -- if
			end -- if
			if n = 0 then
				o.putNL ("No source files to process ...")
			else
				from 
					debug
						create {FileOutput}dumpOutput.init ("_Dump.out")
					end
					if n > 1 then
						o.putLine (n.out + " files to parse ...")
					end -- if
					create {ScreenAndFileOutput}o.init ("_SLang.out")
					create systems.make
					create scriptsToBuild.make
					i := 1
				until
					i > n
				loop
					fName := args.item (i)
					ext := fs.getFileExtension (fName)
					if ext.is_equal (CLangExt) then
						Cmode := True
					elseif ext.is_equal (SLangExt) then
						Cmode := False
					else
						-- Do not parse such file!!!
						o.putLine ("File `" + fName + "` has extension different from a valid SLang source file and ignored")
						skipSourceFile := True
					end -- if
		
					if skipSourceFile then
						skipSourceFile := False
					else
						sName := fs.getFileName(fName)
						create scanner.init (fName)
						if scanner.isReady then
							if Cmode then
								scanner.setCmode
							else
								scanner.setPmode
							end -- if
							create parser.init (scanner, systems, o)
							o.putLine ("Parsing file `" + fName + "`")
							parser.parseSourceFile
							scanner.close
							parser.ast.attach_usage_pool_to_units_and_standalone_routines 
							if parser.ast.statements.count > 0 then
								-- File has anonymous routine - it is a script ! Build mode is to be activated!
								scriptsToBuild.add (fName)
							end -- if  
							if parser.systems /= Void and then parser.systems.count > 0 then
								-- File has description of systems to be built. Build mode is to be activated!
								systems.append (parser.systems)
							end -- if
							debug
								dumpAST (parser, dumpOutput)
							end -- debug

							inspect 
								parser.errorsCount
							when 0 then
								if fs.folderExists (IRfolderName) or else fs.folderCreated (IRfolderName) then
									saveErrCount := parser.ast.saveInternalRepresentation (fName, scanner.timeStamp, sName, ASText, o, Void)
									parser.ast.cutImplementation
									saveErrCount := saveErrCount + parser.ast.saveInternalRepresentation (fName, scanner.timeStamp, sName, INText, o, Void)
									if saveErrCount = 0 then
										-- Remove previous timestamp files and store the latest parsing timestamp !!!
										tsfName := fs.getFilePath(fName) + fs.separator + IRfolderName + fs.separator + fs.getFileName(fName)
										fs.remove_files_with_the_same_name (tsfName)
										tsfName.append_string ("." + scanner.timeStamp.out)
										fs.add_file (tsfName, "r")
										o.putLine ("File `" + fName + "` parsed successfully")
									else
										o.putLine (
											"File `" + fName + 
											"` parsed with no errors. But some parsing results were not stored due to " + 
											saveErrCount.out + " I/O errors!"
										)
										skipBuild := True
									end -- if
								else
									o.putLine (
										"Failed to create folder `" + IRfolderName + 
										"` to store internal files. Parsing results of file `" + fName + "` are not saved!"
									)
									skipBuild := True
								end -- if
							when 1 then
								o.putLine ("File `" + fName + "` parsed with 1 error!")
								skipBuild := True
							else
								o.putLine ("File `" + fName + "` parsed with " + parser.errorsCount.out + " errors!")
								skipBuild := True
							end -- inspect
						else
							o.putLine ("File `" + fName + "` not found or cannot be opened for parsing")
							skipBuild := True
						end -- if
					end -- if
					i := i + 1
				end -- loop
				--if actualFiles = 1 then
				--	o.putLine ("1 file is actual, processing skipped")
				--elseif actualFiles > 1 then
				--	o.putLine (actualFiles.out + " files are actual, processing skipped")
				--end -- if
				n := scriptsToBuild.count
				m := systems.count
				if skipBuild then
					if n > 0 or else m > 0 then
						o.putLine ("Due to parsing errors, all build activities dropped")
					end -- if
				else
					if n > 0 then
						-- N files with anonymous routines to be built
						from 
							i := 1
						until
							i > n
						loop
							fName := scriptsToBuild.item (i)
							--o.putNL ("Building a program from file `" + fName + "`")
							create builder.init (o)
							if builder.build_script_based_program_failed (fName) then
								removeTimeStampFile := True
							end -- if
							i := i + 1
						end -- loop
					end -- if
					if m > 0 then
						-- M system descritions to be built
						from 
							j := 1
							debug
								dumpOutput.putNL ("//----------- System description dump start ------------")
							end -- debug
						until
							j > m
						loop
							sysDsc := systems.item (j)
							debug
								dumpOutput.putNL (sysDsc.out)			
							end -- debug
							create builder.init (o)
							if builder.build_from_system_description_failed (sysDsc) then
								removeTimeStampFile := True
							end -- if
							j := j + 1
						end -- loop
						debug
							dumpOutput.putNL ("//----------- System description dump end  ------------")
						end -- debug
					end -- if
					if removeTimeStampFile then
						safe_delete_file (tsfName)
						removeTimeStampFile := False
						tsfName := Void
					end -- if
				end -- if
			end -- if
		end -- if
		o.putNL ("")
		debug
			dumpOutput.close
		end
		o.close
	end -- init

	safe_delete_file (fName: String) is
	require
		non_void_fName: fName /= Void
	local
		wasError: Boolean
	do
		if not wasError then
			fs.remove_file (fName)
		end -- if
	rescue
		wasError := True
		retry
	end -- safe_delete_file
	
	dumpAST (parser: SLang_parser; dumpOutput: Output) is
	require
		parser_not_void: parser /= Void
	local
		str: String
		i, n: Integer
		j, m: Integer
	do
		if parser.ast.units.count > 0 or else 
			parser.ast.useConst.count > 0 or else
			parser.ast.statements.count > 0 or else
			parser.ast.routines.count > 0 or else
			parser.ast.units.count > 0
		then
			--dumpOutput.putNL ("//-------------- IR dump per file start -------------------")
			m := parser.ast.units.count
			if m > 0 then
				from
					j := 1
					if m = 1 then
						dumpOutput.put ("/* 1 unit compiled successfully: ")
					else
						dumpOutput.put ("/* " + m.out + " units compiled successfully: ")
					end -- if
				until
					j > m
				loop
					dumpOutput.putInToLine (parser.ast.units.item (j).name)
					if j < m then
						dumpOutput.putInToLine (", ")
					end
					if j \\ 12 = 0 then
						dumpOutput.newLine  
					end -- if
					j := j + 1
				end -- loop
				dumpOutput.put ("*/")
				dumpOutput.newLine  
			end -- if
			m := parser.ast.useConst.count
			if m > 0 then
				from
					dumpOutput.putNL ("// Constants import")
					dumpOutput.putInToLine  ("use const ")
					j := 1
				until
					j > m
				loop
					dumpOutput.putInToLine (parser.ast.useConst.item (j))
					if j < m then
						dumpOutput.put (", ")
					end
					j := j + 1
				end -- loop
				o.newLine
			end -- if
			if parser.ast.statements /= Void then
				m := parser.ast.statements.count
				if m > 0 then
					from
						dumpOutput.putNL ("// Anonymous routine")
						j := 1
					until
						j > m
					loop
						str := parser.ast.statements.item (j).out
						dumpOutput.put (str)
						if str.item(str.count) /= '%N' then
							dumpOutput.newLine
						end -- if
						j := j + 1
					end -- loop
					m := parser.ast.typePool.count
					if m > 0 then
						from
							if m = 1 then
								dumpOutput.put ("/* Anonymous routine depends on 1 type: ")
							else
								dumpOutput.put ("/* Anonymous routine depends on " + m.out + " types: ")
							end -- if
							j := 1
						until
							j > m
						loop
							--dumpOutput.putArray (<<"(", parser.ast.typePool.item (j).weight, ")">>)
							dumpOutput.put (parser.ast.typePool.item (j).out)
							if j < m then
								dumpOutput.put(", ")
							end -- if
							if j \\ 7 = 0 then
								dumpOutput.newLine						
							end -- if
							j := j + 1
						end -- loop
						dumpOutput.put ("*/")
						dumpOutput.newLine
					end -- if			
				end -- if
			end -- if
			m := parser.ast.routines.count
			if m > 0 then
				from
					if m = 1 then
						dumpOutput.putNL ("// Standalone routine")
					else
						dumpOutput.putNL ("// " + m.out + " standalone routines")
					end -- if
					j := 1
				until
					j > m
				loop
					str := parser.ast.routines.item (j).out
					dumpOutput.put (str)
					if str.item(str.count) /= '%N' then
						dumpOutput.newLine
					end -- if
					j := j + 1
				end -- loop
				m := parser.ast.rtn_typePool.count
				if m > 0 then
					from
						if m = 1 then
							dumpOutput.put ("/* Standalone routine(s) depends on 1 type: ")
						else
							dumpOutput.put ("/* Standalone routine(s) depends on " + m.out + " types: ")
						end -- if
						j := 1
					until
						j > m
					loop
						--dumpOutput.putArray (<<"(", parser.ast.rtn_typePool.item (j).weight, ")">>)
						dumpOutput.put (parser.ast.rtn_typePool.item (j).out)
						if j < m then
							dumpOutput.put(", ")
						end -- if
						if j \\ 7 = 0 then
							dumpOutput.newLine						
						end -- if
						j := j + 1
					end -- loop
					dumpOutput.put ("*/")
					dumpOutput.newLine
				end -- if			
			end -- if

			m := parser.ast.units.count
			if m > 0 then
				from
					if m = 1 then
						dumpOutput.putNL ("// Type")
					else
						dumpOutput.putNL ("// " + m.out + " types")
					end -- if
					j := 1
				until
					j > m
				loop
					str := parser.ast.units.item (j).out
					dumpOutput.put (str)
					if str.item(str.count) /= '%N' then
						dumpOutput.newLine
					end -- if

					n := parser.ast.units.item(j).typePool.count
					if n > 0 then
						from
							if n = 1 then
								dumpOutput.put ("/* Unit `" + parser.ast.units.item(j).fullUnitName + "` depends on 1 type: ")
							else
								dumpOutput.put ("/* Unit `" + parser.ast.units.item(j).fullUnitName + "` depends on " + n.out + " types: ")
							end -- if
							i := 1
						until
							i > n
						loop
							--dumpOutput.putArray (<<"(", parser.ast.units.item(j).typePool.item (i).weight, ")">>)
							dumpOutput.put (parser.ast.units.item(j).typePool.item (i).out)
							--dumpOutput.put ("/")
							--dumpOutput.put (parser.ast.units.item(j).typePool.item (i).generating_type)
							if i < n then
								dumpOutput.put(", ")
							end -- if
							if i \\ 7 = 0 then
								dumpOutput.newLine						
							end -- if
							i := i + 1
						end -- loop
						dumpOutput.put ("*/")
						dumpOutput.newLine
					end -- if

					--n := parser.ast.units.item(j).stringPool.count
					--if n > 0 then
					--	from
					--		dumpOutput.put ("/* Strings pool:")
					--		i := 1
					--	until
					--		i > n
					--	loop
					--		dumpOutput.put (" ")
					--		dumpOutput.put (parser.ast.units.item(j).stringPool.item (i))
					--		i := i + 1
					--	end -- loop
					--	dumpOutput.put ("*/")
					--	dumpOutput.newLine
					--end -- if

					j := j + 1
				end -- loop
			end -- if
			--dumpOutput.putNL ("//-------------- IR dump per file end ---------------------")
		end -- if
	end -- dumpAST

	doCompilation (sources: Array [String]) is
	require
		non_void_source: sources /= Void
	local
		i, n: Integer
		j, m: Integer
		compilationUnits: Array [CompilationUnitAST]
		compilationUnitIR: CompilationUnitIR
	do
		from
			i := sources.lower
			n := sources.upper
		until
			i > n
		loop
			compilationUnits := parseSource (sources.item (i))
			if compilationUnits /= Void then
				from
					j := compilationUnits.lower
					m := compilationUnits.upper
				until
					j > m
				loop
					compilationUnitIR := compilationUnits.item (j).checkValidity
					if compilationUnitIR /= Void then
						 compilationUnitIR.processIR
					end -- if
					j := j + 1
				end -- loop		
			end -- if
			i := i + 1
		end -- loop
	end -- doCompilation
	
	parseSource (fileName: String): Array [CompilationUnitAST] is
	require
		non_void_file_name: fileName /= Void
	do
	end -- parseSource	
	
end -- class SLangCompiler

deferred class CompilationUnitAST
feature
	checkValidity: CompilationUnitIR is
	deferred
	end -- checkValidity
end -- class CompilationUnitAST
deferred class CompilationUnitIR
feature
	processIR is
	deferred
	end -- processIR
end -- class CompilationUnitIR
