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
		scriptDsc : CompilationUnitAnonymousRoutine
		sysDsc: SystemDescriptor
		fileName: String
		typePool: Sorted_Array[TypeDescriptor]
		typeDsc: TypeDescriptor
		aliasTypes: Sorted_Array [AliasedTypeDescriptor]
		attachedTypeDsc: AttachedTypeDescriptor
		unitDclDsc: UnitDeclarationDescriptor
		statements: Array [StatementDescriptor]
		stmtDsc: StatementDescriptor
		generators: Array [CodeGenerator]
		i, n: Integer
		j, m: Integer
		unitAliasDsc: UnitAliasDescriptor
		cntTypDsc: ContextTypeDescriptor		
		aliasesCount: Integer
	do
		if fs.folderExists (IRfolderName) then
			-- Build the system			
			create scriptDsc.make (Void)
			fileName := IRfolderName + "\_" + fs.getFileName(fName) + ScriptSuffix + "." + ASText
			if scriptDsc.AnonymousRoutineIR_Loaded (fileName, o) then
				o.putNL ("Building a program from file `" + fName + "`")
				-- 1. Build system description - where to look for units !!! 
				create sysDsc.init_script (fs.getFileName(fName), getAnonymousRoutineClusters, Void)
				scriptDsc.attachSystemDescription (sysDsc)
				
				-- 2. Process pools - ensure that all units' interfaces used are loaded
				-- All use const types are registred in the type pool
				--from
				--	useConst  := scriptDscDsc.useConst
				--	n := useConst.count
				--	i := 1
				--until
				--	i > n
				--loop
				--	if useConst.item(i).isNotLoaded (scriptDscDsc) then
				--		skipCodeGen := True
				--	end -- if
				--	i := i + 1
				--end -- loop
				
				from
					typePool := scriptDsc.typePool
					create aliasTypes.make
					n := typePool.count
					i := 1
				until
					i > n					
				loop
					typeDsc := typePool.item(i) 
					debug
						--o.putNL (">>> Checking if type `" + typeDsc.out + "` is loaded")
					end -- debug
					if typeDsc.isNotLoaded (scriptDsc, o) then
						debug
							--o.putNL ("<<< Failed to load `" + typeDsc.out + "`")
						end -- debug
						Result := True
					elseif typeDsc.aliasName /= Void then
						attachedTypeDsc ?= typeDsc
						if attachedTypeDsc /= Void then
							check
								unit_was_loaded: attachedTypeDsc.unitDeclaration /= Void
								unit_registered: sysDsc.allUnits.seek (attachedTypeDsc.unitDeclaration) > 0
							end 
							create unitAliasDsc.init (typeDsc.aliasName, attachedTypeDsc.unitDeclaration)
							if not sysDsc.allUnits.added (unitAliasDsc) then
								o.putNL ("Error: at least two types has the same name `" + typeDsc.aliasName + "`")
								Result := True								
							end -- if				
						end -- if																	
					end -- if
					i := i + 1
				end -- loop

				if not Result then					
					-- Need to check that no name clashes between all alises and already loaded units !!!
					from
						n := sysDsc.allUnits.count
						i := 1
					until
						i > n					
					loop
						cntTypDsc := sysDsc.allUnits.item (i)
						unitAliasDsc ?= cntTypDsc
						if unitAliasDsc = Void then
							-- No more aliases!
							i := n + 1
						else
							aliasesCount := aliasesCount + 1
							create unitDclDsc.makeForSearch (unitAliasDsc.aliasName, Void)							
							if sysDsc.allUnits.seek (unitDclDsc) > 0 then -- already registered
								o.putNL ("Error: type alias `" + unitAliasDsc.aliasName + "` clashes with other unit name")
								Result := True
							end -- if
							i := i + 1								
						end -- if				
					end -- loop						
					
					------ Register all alias types 
					--from
					--	--typePool := scriptDsc.typePool
					--	n := aliasTypes.count
					--	i := 1
					--until
					--	i > n					
					--loop
					--	--	typePool.add (aliasTypes.item (i))
					--	aliasTypeDsc := aliasTypes.item (i)
					--	utcDsc ?= aliasTypeDsc.actualType
					--
					--	--if utcDsc = Void then
					--		create unitDclDsc.makeForSearch (aliasTypeDsc.aliasName, Void)
					--	--else
					--	--	create unitDclDsc.makeForSearch (aliasTypeDsc.aliasName, sysDsc.getFormalGenerics(utcDsc.generics))
					--	--end -- if
					--	
					--	-- unitDclDsc should refer to the actualType !!! Alias support is not consistent !!!
					--	-- change the type of context types !!!   Alias for generic and non-generic - Not_implement_yet !!!
					--
					--	pos	:= sysDsc.allUnits.seek (unitDclDsc)
					--	--pos	:= sysDsc.allUnits.seek (aliasTypeDsc)
					--	if pos > 0 then -- already registered
					--		o.putNL ("Warning: type alias `" + typeDsc.aliasName + "` clashes with other unit name and will be ignored")
					--	else -- have it registered
					--		--sysDsc.allUnits.add_after (aliasTypeDsc, pos)
					--		sysDsc.allUnits.add_after (unitDclDsc, pos)
					--	end -- if					
					--
					--	i := i + 1
					--end -- loop

					o.putLine ((sysDsc.allUnits.count - aliasesCount).out + " units loaded")
					o.newLine
					debug
						sysDsc.dumpContext (o)						
					end -- debug
					
					if sysDsc.contextValidated (o) then
						-- If all required types loaded and validated
						-- 3. Check validity of cuDsc.statements
						from
							statements := scriptDsc.statements
							check
								non_void_statements: statements /= Void
							end -- check
							n := statements.count
							i := 1
						until
							i > n
						loop
							if statements.item(i).isInvalid (scriptDsc, o) then
								debug
									o.putNL ("Statement `" + statements.item(i).out + "` invalid!")
								end -- debug
								Result := True
							end -- if
							i := i + 1
						end -- loop
					else
						Result := True
					end -- if
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
							statements := scriptDsc.statements
							n := statements.count
							--check
							--	valid_statements_not_void: valid_statements /= Void
							--	counter_consistent: n = valid_statements.count
							--end -- check
							i := 1
						until
							i > n
						loop
							stmtDsc := statements.item(i)
							--validStmtDsc := valid_statements.item (i)
							from
								j := 1
							until
								j > m
							loop
								stmtDsc.generate (generators.item (j))
								--validStmtDsc.generate (generators.item (j))
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
		folderName: String -- short name of the folder where artifacts be stored
	do
		folderName := "_$" + sysDsc.name
		if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
			-- Build the system
			-- All 'from' and 'clusters' sources are to be parsed 
			if sysDsc.sourcesActual (o) then
				if sysDsc.entry = Void then
					-- Library
					o.putNL ("Building library `" + sysDsc.name + "`")
					Result := library_build_failed (sysDsc, folderName)	
				else
					-- Executable with the entry point
					o.putNL ("Building executable `" + sysDsc.name + "`")
					Result := executable_build_failed (sysDsc, folderName)
				end -- if
			else
				Result := True
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
		-- C-code generation activation
		create {C_CodeGenerator}codeGenerator.init (outputFileName + ".c", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "Generation 'C' failed to start")
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

	executable_build_failed (sysDsc: SystemDescriptor; folderName: String): Boolean is
	require
		non_void_system_dsc: sysDsc /= Void
		is_executable: sysDsc.entry /= Void and sysDsc.from_paths = Void
		non_void_folder_name: folderName /= Void
	local
		entryPointName: String
		clusters: Array [ClusterDescriptor]
		rootDsc: ContextTypeDescriptor
		rootUnitDsc: UnitDeclarationDescriptor
		typesPool: Sorted_Array[TypeDescriptor]
		index: Integer
	do
		-- Find an entry point
		entryPointName := sysDsc.entry
		if entryPointName.item (1) = entryPointName.item (1).as_upper then
			-- Entry point is the unit name! (It cannot be generic BTW)
			clusters := sysDsc.hasUnit (entryPointName)
			if clusters = Void or else clusters.count = 0 then
				-- Such unit is not found in the search universe !!!
				o.putNL ("Error: root unit `" + entryPointName + "` is not found in the provided context")
				Result := True
			elseif clusters.count > 1 then
				-- More than one unit is found in the search universe !!!
				o.putNL ("Error: " + clusters.count.out + " versions of the root unit `" + entryPointName + "` found in the provided context. Select only one to be used")
				Result := True
			else
				-- Load it
				o.putLine ("Loading root unit `" + entryPointName + "`")
				rootDsc := sysDsc.loadUnitInterafceFrom (clusters.item (1).name, entryPointName, o)
				if rootDsc = Void then
					-- There was a problem to load root unit interface 
					o.putNL ("Error: root unit `" + entryPointName + "` was not loaded correctly")
					Result := True
-- Not actual any more - source check was done already
--				elseif fs.file_exists(Result.srcFileName) then
--					-- Check if the type source file was changed after type IR was created. If necessary run the parser. 
--					if fs.file_time(Result.srcFileName).rounded /= Result.timeStamp then
--						-- Ensure source file parsed
--						Result := fileParsedForUnit (Result.srcFileName, o, unitExternalName, Result)
--					end -- if
				else
					rootUnitDsc := rootDsc.getUnitDeclaration -- root unit and its alias if any were loaded and registered		
					typesPool := rootUnitDsc.typePool
					if typesPool /= Void then
						from
							index := typesPool.count
						until
							index > 0
						loop
							if typesPool.item (index).isNotLoaded (sysDsc.context, o) then
								Result := True
							end -- if
							index := index - 1
						end -- loop
					end -- if





not_implemented_yet ("Building executable `" + sysDsc.name + "` from unit `" + sysDsc.entry + "` from cluster `" + clusters.item (1).name + "`") 
				end -- if
			end -- if	
		else
			-- Entry point is the standalone procedure name!
			clusters := sysDsc.hasStandAloneRoutine (entryPointName)
			if clusters = Void or else clusters.count = 0 then
				-- Such unit is not found in the search universe !!!
				o.putNL ("Error: routine `" + entryPointName + "` is not found in the provided context")
				Result := True
			elseif clusters.count > 1 then
				-- More than one unit is found in the search universe !!!
				o.putNL ("Error: " + clusters.count.out + " versions of the routine `" + entryPointName + "` found in the provided context. Select only one to be used")
				Result := True
			else
not_implemented_yet ("Building executable `" + sysDsc.name + "` from standalone procedure `" + sysDsc.entry + "` from cluster `" + clusters.item (1).name + "`")
			end -- if
		end -- if
	end -- executable_build_failed
	
	library_build_failed (sysDsc: SystemDescriptor; folderName: String): Boolean is
		-- short name of the folder where artifacts be stored
	require
		non_void_system_dsc: sysDsc /= Void
		is_library: sysDsc.entry = Void and sysDsc.from_paths /= Void
		non_void_folder_name: folderName /= Void
	local
		from_paths: Sorted_Array [String] -- List of paths to build the library from
		generators: Array [CodeGenerator]
		i: Integer
	do
		from_paths := sysDsc.from_paths -- List of paths to build the library from
		from
			check
				non_void_list_of_paths: from_paths /= Void
			end -- check
			i := from_paths.count
			check
				non_empty_list_of_paths: i > 0
			end -- check
		until
			i <= 0
		loop
			-- Load all interfaces and ensure context is consistent
			if interfacesFromPathNotLoaded (sysDsc, from_paths.item (i)) then
				Result := True
			end -- if
			i := i - 1
		end -- loop

		if not Result then
			generators := initCodeGenerators (folderName + fs.separator + sysDsc.name, false)
			from
				i := from_paths.count
				check
					non_empty_list_of_paths: i > 0
				end -- check
			until
				i <= 0
			loop
				if libraryBuildFailed (sysDsc, from_paths.item (i), generators) then
					Result := True
				end -- if
				i := i - 1
			end -- loop
			closeCodeGenerators (generators)
		end -- if 
	end -- library_build_failed

	interfacesFromPathNotLoaded (sysDsc: SystemDescriptor; path: String): Boolean is
	require
		non_void_sd: sysDsc /= Void
		non_void_path: path /= Void
	local
		ir_path: String
		ast_files: Array [Fsys_Dat]
		fileDsc: Fsys_Dat
		fileName: String
		cuUnitDsc: CompilationUnitUnit
		rtnDsc: CompilationUnitStandaloneRoutine
		i: Integer
	do
		ir_path := path + fs.separator + IRfolderName
		if fs.folderExists (ir_path) then
			debug
				trace ("Loading interfaces from `" + ir_path + "`")
			end -- debug
			from
				ast_files := fs.getAllFilesWithExtension (ir_path, INText)
				i := ast_files.count
			until
				i <= 0
			loop
				fileDsc := ast_files.item (i) 
				fileName := fileDsc.name
				if fileName.has_substring (UnitSuffix) then
					if not fileName.has_substring (AliasPrefix) then -- skip alias file
						create cuUnitDsc.make (Void)
						if cuUnitDsc.UnitIR_Loaded (fileDsc.path, o) then
							debug
								trace ("Unit `" + cuUnitDsc.unitDclDsc.fullUnitName + "` loaded from file `" + ast_files.item (i).path + "`")
							end -- debug
							cuUnitDsc.attachSystemDescription (sysDsc)
							-- ????
							
						else
							Result := True
						end -- if				
					end -- if
				elseif fileName.has_substring (RoutinesSuffix) then 
					create rtnDsc.make (Void)
					if rtnDsc.RoutineIR_Loaded (fileDsc.path, o) then
						rtnDsc.attachSystemDescription (sysDsc)
						debug
							trace ("Standalone routine `" + rtnDsc.routine.name + "` loaded from file `" + ast_files.item (i).path + "`")
						end -- debug
						-- ????
					else
						Result := True
					end -- if				
				end -- if
				i := i - 1
			end -- loop
		end -- if
	end -- interfacesFromPathNotLoaded

	libraryBuildFailed (sysDsc: SystemDescriptor; path: String; generators: Array [CodeGenerator]): Boolean is
	require
		non_void_sd: sysDsc /= Void
		non_void_path: path /= Void
		non_void_list_of_code_generators: generators /= Void
	local
		ir_path: String
		ast_files: Array [Fsys_Dat]
		fileDsc: Fsys_Dat
		fileName: String
		cuUnitDsc: CompilationUnitUnit
		rtnDsc: CompilationUnitStandaloneRoutine
		i, j: Integer
	do
		ir_path := path + fs.separator + IRfolderName
		if fs.folderExists (ir_path) then
			debug
			--	trace ("Load units from `" + ir_path + "`")
			end -- debug
			from
				ast_files := fs.getAllFilesWithExtension (ir_path, ASText)
				i := ast_files.count
			until
				i <= 0
			loop
				fileDsc := ast_files.item (i) 
				fileName := fileDsc.name
				if fileName.has_substring (UnitSuffix) then
					--if not fileName.has_substring (AliasPrefix) then -- There is no IMPL files for aliases !!!
						create cuUnitDsc.make (Void)
						if cuUnitDsc.UnitIR_Loaded (fileDsc.path, o) then
							debug
								--trace ("Type `" + unitDsc.type.fullUnitName + "` loaded from file `" + ast_files.item (i).path + "`")
							end -- debug
							cuUnitDsc.attachSystemDescription (sysDsc)
							
							-- TO REDO! Here load implementation check it and geneatte code !!!
							--if cuUnitDsc.unitDclDsc.isNotLoaded (cuUnitDsc, o) then
							--	Result := True
							--else
							--	debug
							--		--sysDsc.dumpContext (o)
							--	end
								if cuUnitDsc.unitDclDsc.isInvalid (cuUnitDsc, o) then
									Result := True
								else
									from
										j := generators.count
									until
										j <= 0
									loop
										cuUnitDsc.unitDclDsc.generate(generators.item (j))
										j := j - 1
									end -- loop
								end -- if
							--end -- if					
						else
							Result := True
						end -- if				
					--end -- if
				elseif fileName.has_substring (RoutinesSuffix) then 
					create rtnDsc.make (Void)
					if rtnDsc.RoutineIR_Loaded (fileDsc.path, o) then
						rtnDsc.attachSystemDescription (sysDsc)
						debug
							--trace ("Standalone routine loaded from file `" + ast_files.item (i).path + "`")
						end -- debug
						if rtnDsc.routine.isInvalid (rtnDsc, o) then
							Result := True
						else
							from
								j := generators.count
							until
								j <= 0
							loop
								rtnDsc.routine.generate(generators.item (j))
								j := j - 1
							end -- loop
						end -- if
					else
						Result := True
					end -- if				
				end -- if
				i := i - 1
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