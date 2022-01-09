class SLang_compiler
creation
	init
feature {None}
	o: Output
	systems: Sorted_Array [SystemDescriptor]
	fs: FileSystem is
	once
		create Result
	end -- fs
	memory: Memory is
	once
		create Result
	end
	IRfolderName: String is "_$_IR"

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
		scripts: Sorted_Array [String]
		files: Array [FSys_Dat]
		fName: String
		sName: String
		ext: String
		slangFileCount: Integer
		actualFiles: Integer
		skipBuild: Boolean
		skipSourceFile: Boolean
		Cmode: Boolean
		i, n: Integer
		j, m: Integer
		-- It should be commented out in the final version!
		dumpOutput: Output
	do
		create {ScreenOutput}o		
		o.putNL ("SLang compiler v0.99.01 (Build <AVK Jan 5th 2022>)")
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
						if ext.is_equal ("slang") or else ext.is_equal ("clang") then
							if slangFileCount = 0 then
								args.put (fName, 1)
							else	
								args.force (fName, args.count + 1)
							end
							slangFileCount := slangFileCount + 1
						end -- if
						j := j + 1
					end					
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
					create {ScreenAndFileOutput}o.init ("_Slang.out")
					create systems.make
					create scripts.make
					i := 1
				until
					i > n
				loop
					fName := args.item (i)
					ext := fs.getFileExtension (fName)
					if ext.is_equal ("clang") then
						Cmode := True
					elseif ext.is_equal ("slang") then
						Cmode := False
					else
						-- Do not parse such file!!!
						o.putLine ("File `" + fName + "` has extension different from SLang source files. Parsing skipped.")
						skipSourceFile := True
					end -- if
		
					if skipSourceFile then
						skipSourceFile := False
					else
						sName := "_" + fs.getFileName(fName)
	--dumpOutput.putNL ("// " + fName + ":" + fs.file_time (fName).out + " vs. " + 
	--	IRfolderName + "\" + sName + ".Slang#int" + ":" + fs.file_time (IRfolderName + "\" + sName + ".Slang#int").out )
						if fs.younger (fName, IRfolderName + "\" + sName + ".Slang#int") then
							-- No need to parse - interface was created later then last change of the source file
							o.putLine ("File `" + fName + "` was not changed. Parsing skipped.")
							actualFiles := actualFiles + 1
						else
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
								if parser.ast.statements.count > 0 then
									scripts.add (fName)
								end -- if
								if parser.systems /= Void and then parser.systems.count > 0 then
									systems.append (parser.systems)
								end -- if
								debug
									dumpAST (parser, dumpOutput)
								end

								inspect 
									parser.errorsCount
								when 0 then
									if fs.folderExists (IRfolderName) or else fs.folderCreated (IRfolderName) then
										parser.ast.saveInternalRepresentation (IRfolderName + "\", sName, ".Slang#ast", o)
										parser.ast.cutImplementation
										parser.ast.saveInternalRepresentation (IRfolderName + "\", sName, ".Slang#int", o)
										o.putLine ("File `" + fName + "` parsed with no errors!")
									else
										o.putLine ("Failed to create folder `" + IRfolderName + "` to store internal files. Parsing results of file `" + fName + "` are not saved!")
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
								o.putLine ("File `" + fName + "` not found or cannot be opened")
							end -- if
						end -- if
					end -- if
					i := i + 1
				end -- loop
				if actualFiles = 1 then
					o.putLine ("1 file actual, parsing skipped ...")
				elseif actualFiles > 1 then
					o.putLine (actualFiles.out + " files are actual, parsing skipped ...")
				end -- if
				n := scripts.count
				m := systems.count
				if skipBuild then
					if n > 0 or else m > 0 then
						o.putLine ("Due to parsing errors, build is dropped")
					end -- if
				else
					if n > 0 then
						from 
							i := 1
						until
							i > n
						loop
							fName := scripts.item (i)
							--o.putNL ("Building a program from file `" + fName + "`")
							create builder --.init (o)
							builder.build_from_file (fName, fs, o)
							i := i + 1
						end -- loop
					end -- if
					if m > 0 then
						from 
							j := 1
							debug
								dumpOutput.putNL ("//----------- System description dump start ------------")
							end
						until
							j > m
						loop
							sysDsc := systems.item (j)
							debug
								dumpOutput.putNL (sysDsc.out)			
							end
							create builder
							builder.build (sysDsc, fs, o)
							j := j + 1
						end -- loop
						debug
							dumpOutput.putNL ("//----------- System description dump end  ------------")
						end
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
	
	dumpAST (parser: SLang_parser; dumpOutput: Output) is
	require
		parser_not_void: parser /= Void
	local
		str: String
		j, m: Integer
	do
		if parser.ast.units.count > 0 or else 
			parser.ast.useConst.count > 0 or else
			parser.ast.statements.count > 0 or else
			parser.ast.routines.count > 0 or else
			parser.ast.units.count > 0
		then
			dumpOutput.putNL ("//-------------- IR dump per file start -------------------")
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
			end -- if
			m := parser.ast.units.count
			if m > 0 then
				from
					if m = 1 then
						dumpOutput.putNL ("// Unit")
					else
						dumpOutput.putNL ("// " + m.out + " units")
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
					j := j + 1
				end -- loop
			end -- if
			m := parser.ast.typePool.count
			if m > 0 then
				from
					if m = 1 then
						dumpOutput.put ("/* Depends on 1 type: ")
					else
						dumpOutput.put ("/* Depends on " + m.out + " types: ")
					end -- if
					j := 1
				until
					j > m
				loop
					dumpOutput.putArray (<<"(", parser.ast.typePool.item (j).weight, ")">>)
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
			dumpOutput.putNL ("//-------------- IR dump per file end ---------------------")
		end -- if
	end -- dumpAST
	
end -- class SLang_compiler