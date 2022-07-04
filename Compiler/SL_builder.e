class SLang_builder
inherit
	SLangConstants
	Server
create
	init
feature {None}
	env: Environment is
	once
		create Result
	end -- env
	getAnonymousRoutineClusters: Sorted_Array [ClusterDescriptor] is
	local	
		libPath: String
		clusterDsc: ClusterDescriptor
		start, stop: Integer
	do
		create clusterDsc.init (".", Void, Void, Void)
		create Result.fill (<<clusterDsc>>) -- current folder is always used to look for units
		-- Add what should be added from the environemnt ...
		libPath := env.env_item (SLNG_LIB)
		if libpath /= Void then
			from
				start := 1
				stop := libPath.index_of (';', start) 
			until
				stop = 0
			loop
				create clusterDsc.init (libPath.substring (start, stop - 1), Void, Void, Void)
				Result.add (clusterDsc)
				start := stop + 1
				stop := libPath.index_of (';', start) 
			end -- loop
			if start = 1 then
				create clusterDsc.init (libPath, Void, Void, Void)
				Result.add (clusterDsc)
			end -- if
		end -- if
	end -- getAnonymousRoutineClusters
feature {Any}
	
	build_script_based_program_failed (fName: String): Boolean is
	require
		non_void_file_name: fName /= Void
	local 
		cuDsc : CompilationUnitAnonymousRoutine
		sysDsc: SystemDescriptor
		fileName: String
		typePool: Sorted_Array[TypeDescriptor]
		typeDsc: TypeDescriptor
		aliasTypes: Sorted_Array [AliasedTypeDescriptor]
		attachedTypeDsc: AttachedTypeDescriptor
		--unitTypeDsc: UnitTypeNameDescriptor
		aliasTypeDsc: AliasedTypeDescriptor
		statements: Array [StatementDescriptor]
		stmtDsc: StatementDescriptor
		generators: Array [CodeGenerator]
		i, n: Integer
		j, m: Integer
	do
		if fs.folderExists (IRfolderName) then
			-- Build the system			
			create cuDsc.init (Void)
			fileName := IRfolderName + "\_" + fs.getFileName(fName) + ScriptSuffix + "." + ASText
			if cuDsc.AnonymousRoutineIR_Loaded (fileName, o) then
				o.putNL ("Building a program from file `" + fName + "`")
				-- 1. How to get system description - where to look for units !!! 
				create sysDsc.init_script (fs.getFileName(fName), getAnonymousRoutineClusters, Void)
				cuDsc.attachSystemDescription (sysDsc)
				
				-- 2. Process pools - ensure that all units' interfaces used are loaded
				-- All use const types are registred in the type pool
				--from
				--	useConst  := cuDsc.useConst
				--	n := useConst.count
				--	i := 1
				--until
				--	i > n
				--loop
				--	if useConst.item(i).isNotLoaded (cuDsc) then
				--		skipCodeGen := True
				--	end -- if
				--	i := i + 1
				--end -- loop
				from
					typePool := cuDsc.typePool
					create aliasTypes.make
					n := typePool.count
					i := 1
				until
					i > n					
				loop
debug
--	o.putNL ("Type pool: " + i.out + " - `" + typePool.item(i).out + "` loading!")
end -- debug
					typeDsc := typePool.item(i) 
					if typeDsc.isNotLoaded (cuDsc, o) then
						debug
							--o.putNL ("Load interface of `" + typePool.item(i).out + "` failed!")
						end -- debug
						Result := True
					elseif typeDsc.aliasName /= Void then
						attachedTypeDsc ?= typeDsc
						if attachedTypeDsc /= Void then
							create aliasTypeDsc.init (typeDsc.aliasName, attachedTypeDsc)
							if not aliasTypes.added (aliasTypeDsc) then
	-- not_implemented_yet: It should be a validity error !!!
								o.putNL ("Error: at least two type types has the same alias `" + typeDsc.aliasName + "`")
								Result := True
							end -- if
						end -- if
					end -- if
					i := i + 1
				end -- loop

				if not Result then
					-- Register all alias types
					from
						typePool := cuDsc.typePool
						n := aliasTypes.count
						i := 1
					until
						i > n					
					loop
						typePool.add (aliasTypes.item (i))
						i := i + 1
					end -- loop
					
					-- If all required types loaded 
					-- 3. Check validity of cuDsc.statements
					from
						statements := cuDsc.statements
						n := statements.count
						i := 1
					until
						i > n
					loop
						if statements.item(i).isInvalid (cuDsc, o) then
							debug
								o.putNL ("Statement `" + statements.item(i).out + "` invalid!")
							end -- debug
							Result := True
						end -- if
						i := i + 1
					end -- loop
				end -- if
				
				if Result then
					o.putNL ("Info: code generation skipped due to errors found")
				else
					-- 4. Generate code for cuDsc.statements
					--generators := initCodeGenerators (IRfolderName + "\_" + fs.getFileName(fName), true)			
					generators := initCodeGenerators (fs.getFileName(fName), true)			
					m := generators.count
					if m > 0 then
						from
							statements := cuDsc.statements
							n := statements.count
							i := 1
						until
							i > n
						loop
							stmtDsc := statements.item(i)
							from
								j := 1
							until
								j > m
							loop
								stmtDsc.generate (generators.item (j))
								j := j + 1
							end -- loop
							i := i + 1
						end -- loop
						closeCodeGenerators (generators)
						-- 5. Link !!!! How ????
					else
						-- No generators
						o.putNL ("Consistency error: No code generators available")
					end -- if
				end -- if
			else
				Result := True
				-- Allready printed!
				--	-- AST not loaded !!!
				--	o.putNL ("Error: unable to load compiled module from file `" + fileName + "`")
			end -- if
		else
			Result := True
			o.putNL ("Error: SLang folder with artefacts `" + IRfolderName + "` not found")
		end -- if
	end -- build_script_based_program_failed

	build_from_system_description_failed (sysDsc: SystemDescriptor): Boolean is
	require
		non_void_sd: sysDsc /= Void
	local 
		folderName: String -- name of the folder where object files be stored
	do
		folderName := "_$"
		if sysDsc.name.is_equal ("*") then
			folderName.append_string ("_OBJ")
		else
			folderName.append_string ("$" + sysDsc.name)
		end -- if
		if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
			-- Build the system
			-- All 'from' sources are to be parsed 
			sysDsc.checkSources (o)
			if sysDsc.entry = Void then
				-- Library
				o.putNL ("Building library `" + sysDsc.name + "`")
				Result := library_build_failed (sysDsc)	
			elseif sysDsc.name.is_equal ("*") then
				-- Set of object files from all units and routines of the current folder
				o.putNL ("Build for all source files")
				Result := obj_files_build_failed (sysDsc)
--not_implemented_yet ("Building all files")
			else
				-- Executable with the entry point
				o.putNL ("Building executable `" + sysDsc.name + "`")
				Result := executable_build_failed (sysDsc)
--not_implemented_yet ("Building executable `" + sysDsc.name + "`")
			end -- if
		else
			Result := True
			o.putNL ("Error: Not able to create SLang folder with artefacts  `" + folderName + "`")
		end -- if
	end -- build_from_system_description_failed

feature {None}

	initCodeGenerators (outputFileName: String; buildExecutable: Boolean): Array [CodeGenerator] is
	require
		non_void_file_name: outputFileName /= Void
	local
		codeGenerator: CodeGenerator	
	do
		create Result.make (1, 0)		
		-- LLVM Windows generation activation
		if buildExecutable then
			create {LLVM_CodeGenerator}codeGenerator.init (outputFileName + ".exe", "x86_64-pc-windows-msvc", buildExecutable)
		else
			create {LLVM_CodeGenerator}codeGenerator.init (outputFileName + ".lib", "x86_64-pc-windows-msvc", buildExecutable)
			-- not_implemented_yet .dll !!!
		end -- if
		registerCodeGenerator (codeGenerator, Result, "Generation 'LLVM - x86_64-pc-windows-msvc' failed to start")
		-- LLVM Linux generation activation
		create {LLVM_CodeGenerator}codeGenerator.init (outputFileName, "x86_64-pc-linux-gnu", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "Generation 'LLVM - x86_64-pc-linux-gnu' failed to start")
		-- MSIL generation activation
		create {MSIL_CodeGenerator}codeGenerator.init (outputFileName + ".msil", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "Generation 'MSIL' failed to start")
		-- JVM generation activation
		create {JVM_CodeGenerator}codeGenerator.init (outputFileName + ".class", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "Generation 'JVM' failed to start")
		-- ARK generation activation
		create {ARK_CodeGenerator}codeGenerator.init (outputFileName + ".abc", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "Generation 'ARK' failed to start")
		create {C_CodeGenerator}codeGenerator.init (outputFileName + ".c", buildExecutable)
		--registerCodeGenerator (codeGenerator, Result, "Generation 'C' failed to start")
	ensure
		non_void_list_of_code_generators: Result /= Void
	end -- initCodeGenerators
	
	closeCodeGenerators (generators: Array [CodeGenerator]) is
	require
		non_void_list_of_code_generators: generators /= Void
	local
		j, m: Integer
	do
		from
			j := 1
		until
			j > m
		loop
			generators.item (j).dispose
			j := j + 1
		end -- loop
	end -- closeCodeGenerators

	obj_files_build_failed (sysDsc: SystemDescriptor): Boolean is
	require
		non_void_system_dsc: sysDsc /= Void
		is_executable: (sysDsc.entry /= Void and then sysDsc.entry.item (1) = '*') and sysDsc.from_paths = Void
	local
	do
		Result := library_build_failed(sysDsc)
	end -- obj_files_build_failed

	executable_build_failed (sysDsc: SystemDescriptor): Boolean is
	require
		non_void_system_dsc: sysDsc /= Void
		is_executable: sysDsc.entry /= Void and sysDsc.from_paths = Void
	local
		entryPointName: String
		clusters: Array [ClusterDescriptor]
	do
		-- Find entry point.
		entryPointName := sysDsc.entry
		if entryPointName.item (1) = entryPointName.item (1).as_upper then
			-- Entry point is the type name!
			clusters := sysDsc.hasUnit (entryPointName)
			if clusters = Void or else clusters.count = 0 then
				-- Such type is not found in the search universe !!!
				o.putNL ("Error: root type `" + entryPointName + "` is not found in the provided universe")
			elseif clusters.count > 1 then
				-- More than one type is found in the search universe !!!
				o.putNL ("Error: " + clusters.count.out + " versions of the root type `" + entryPointName + "` found in the provided universe. Select only one to be used")
			else
not_implemented_yet ("Building executable `" + sysDsc.name + "` from type `" + sysDsc.entry + "`")
				-- Load it
--				o.putLine ("Loading interface of the root `" + entryPointName + "`")
--				Result := loadUnitInterafceFrom (clusters.item (1).name, unitExternalName, o)
--				if Result = Void then
--					-- There was a problem to load type interface 
--					o.putNL ("Error: type `" + unitPrintableName + "` was not loaded correctly")
--				elseif fs.file_exists(Result.srcFileName) then
--					-- Check if the type source file was changed after type IR was created. If necessary run the parser. 
--					if fs.file_time(Result.srcFileName).rounded /= Result.timeStamp then
--						-- Ensure source file parsed
--						Result := fileParsedForUnit (Result.srcFileName, o, unitExternalName, Result)
--					end -- if
--				else
--					o.putNL ("Warning: source file for the root type `" + unitPrintableName + "` is no longer in place")
--				end -- if
			end -- if	
		else
not_implemented_yet ("Building executable `" + sysDsc.name + "` from standalone procedure `" + sysDsc.entry + "`")
			-- Entry point is the standaloe procedure name!
			--sysDsc. findStandAloneRoutine
		end -- if
	end -- executable_build_failed
	
	library_build_failed (sysDsc: SystemDescriptor): Boolean is
	require
		non_void_system_dsc: sysDsc /= Void
		is_library: sysDsc.entry = Void and sysDsc.from_paths /= Void
	local
		folderName: String -- name of the folder where object files be stored
		from_paths: Sorted_Array [String] -- List of paths to build the library from
		generators: Array [CodeGenerator]
		i, n: Integer
	do
		folderName := "_$"
		if sysDsc.name.is_equal ("*") then
			-- Just set of object files to be built
			folderName.append_string ("_OBJ")
		else
			-- All object files are to be put into the library one
			folderName.append_string ("$" + sysDsc.name)
		end -- if
		if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
			generators := initCodeGenerators (folderName + fs.separator + sysDsc.name, false)
			from_paths := sysDsc.from_paths -- List of paths to build the library from
			from
				i := 1
				n := from_paths.count
				check
					non_empty_list_of_paths: n > 0
				end -- check
			until
				i > n
			loop
				if libraryBuildFailed (sysDsc, from_paths.item (i), generators) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
			closeCodeGenerators (generators)
		else
			Result := True
			o.putNL ("Error: Not able to create SLang folder with artefacts  `" + folderName + "`")
		end -- if
	end -- library_build_failed

	libraryBuildFailed (sysDsc: SystemDescriptor; path: String; generators: Array [CodeGenerator]): Boolean is
	require
		non_void_sd: sysDsc /= Void
		non_void_path: path /= Void
		non_void_list_of_code_generators: generators /= Void
	local
		ir_path: String
		ast_files: Array [Fsys_Dat]
		fileDsc: Fsys_Dat
		unitDsc: CompilationUnitUnit
		rtnDsc: CompilationUnitStandaloneRoutine
		--rtnsDsc: CompilationUnitStandaloneRoutines
		--routines: Sorted_Array [StandaloneRoutineDescriptor]
		i, n: Integer
		j, m: Integer
		--k, l: Integer
	do
		ir_path := path + fs.separator + IRfolderName
		if fs.folderExists (ir_path) then
debug
--	trace ("Load units from `" + ir_path + "`")
end -- debug
			from
				ast_files := fs.getAllFilesWithExtension (ir_path, ASText)
				i := 1
				n := ast_files.count
			until
				i > n
			loop
				fileDsc := ast_files.item (i) 
				if fileDsc.name.has_substring (UnitSuffix) then
					create unitDsc.make 
					if unitDsc.UnitIR_Loaded (fileDsc.path, o) then
debug
	trace ("Unit `" + unitDsc.type.fullUnitName + "` loaded from file `" + ast_files.item (i).path + "`")
end -- debug
	-- How to ignore alias code ... Or process it ....
	--					if cuDsc.type.name.is_equal () then
							-- That is not alias code
							unitDsc.attachSystemDescription (sysDsc)
							if unitDsc.type.isInvalid (unitDsc, o) then
								Result := True
							else
								from
									j := 1
									m := generators.count
								until
									j > m
								loop
									unitDsc.type.generate(generators.item (j))
									j := j + 1
								end -- loop
							end -- if
	--					end -- if
					else
						Result := True
					end -- if				
				elseif fileDsc.name.has_substring (RoutinesSuffix) then 
					create rtnDsc.make 
					if rtnDsc.RoutineIR_Loaded (fileDsc.path, o) then
						rtnDsc.attachSystemDescription (sysDsc)
debug
	trace ("Standalone routine loaded from file `" + ast_files.item (i).path + "`")
end -- debug
						if rtnDsc.routine.isInvalid (rtnDsc, o) then
							Result := True
						else
							from
								j := 1
								m := generators.count
							until
								j > m
							loop
								rtnDsc.routine.generate(generators.item (j))
								j := j + 1
							end -- loop
						end -- if

						--from
						--	routines := rtnsDsc.routines
						--	k := 1
						--	l := routines.count
						--until
						--	k > l
						--loop
						--	if routines.item (k).is_invalid (unitDsc, o) then
						--		Result := True
						--	else
						--		from
						--			j := 1
						--			m := generators.count
						--		until
						--			j > m
						--		loop
						--			routines.item (k).generate(generators.item (j))
						--			j := j + 1
						--		end -- loop
						--	end -- if
						--	k := k + 1
						--end -- loop
					else
						Result := True
					end -- if				
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- libraryBuildFailed

trace (message: String ) is
do
	o.putNL (">>> " + message)
end -- trace

not_implemented_yet (featureName: String) is
require
	feature_name_not_void : featureName /= Void
do
	o.putNL ("NOT IMPLEMENTED YET <" + featureName + ">")
end -- not_implemented_yet

	o: Output
	init(anOutput: like o) is
	require
		non_void_output: anOutput /= Void	
	do
		o := anOutput
	end -- init

	registerCodeGenerator (codeGenerator: CodeGenerator; generators:Array [CodeGenerator]; errorMessage: String) is
	do
		inspect
			codeGenerator.status
		when 0 then
			-- do nothing as code generator is not enabled yet
		when -1 then
			o.putNL (errorMessage)
		when 1 then
			generators.force (codeGenerator, generators.count + 1)
		end -- inspect
	end
end -- class SLang_builder