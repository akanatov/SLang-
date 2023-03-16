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

	checkStatementsValidity (statements: Array [StatementDescriptor]; scriptDsc : CompilationUnitAnonymousRoutine): Boolean is
	require
		non_void_statements: statements /= Void	
	local
		i, n: Integer
	do
		from
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
	end -- checkStatementsValidity
	
	build_script_based_program_failed (fName: String): Boolean is
	require
		non_void_file_name: fName /= Void
	local 
		scriptCU : CompilationUnitAnonymousRoutine
		sysDsc: SystemDescriptor
		fullFileName: String
		outputName: String
		folderName: String
	do
		if fs.folderExists (IRfolderName) then
			outputName := fs.getFileName(fName)
			-- Build the system			
			create scriptCU.make (Void)
			fullFileName := IRfolderName + fs.separator + "_" + outputName + ScriptSuffix + "." + ASText
			if scriptCU.AnonymousRoutineIR_Loaded (fullFileName, o) then
				o.putNL ("Building a program from file `" + fName + "`")
				-- 1. Build system description - where to look for units !!! 
				create sysDsc.init_script (outputName, getAnonymousRoutineClusters, Void)
				scriptCU.attachSystemDescription (sysDsc)
				-- 1.1 Check that spources are actual
				if sysDsc.sourcesActual (o) then
					-- 2.Load all units used
					Result := failedToLoadRequiredTypes (sysDsc, scriptCU.typePool)
					debug
						sysDsc.dumpContext (o)						
					end -- debug
					if not Result then
						-- 3. Check project context validity
						if sysDsc.allUnitInterfacesAreValid (o) then
							-- If all required types loaded and validated
							-- 4. Check validity of cuDsc.statements
							Result := checkStatementsValidity (scriptCU.statements, scriptCU)
						else
							Result := True
						end -- if
					end -- if
					
					if Result then
						o.putNL ("Info: code generation skipped due to errors found")
					else
						folderName := "_$" + outputName
						if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
							-- 5. Generate code for cuDsc.statements
							Result := generationFailed (scriptCU, folderName + fs.separator + outputName)
							if not Result then
								not_implemented_yet ("Building executable `" + sysDsc.name + "` from script `" + fname + "`%N")
								-- 6.Check validity and code generation for used CUs
								-- 7. Build executable
							end -- if
						else
							Result := True
							o.putNL ("Error: project folder `" + folderName + "` cannot be created")
						end -- if
					end -- if
				else
					Result := True
					o.putNL ("Info: compilation aborted due to parser error")
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
			-- All 	'from' and 'clusters' sources are to be parsed 
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
		registerCodeGenerator (codeGenerator, Result, "'LLVM - x86_64-pc-windows-msvc' generation failed to start")
		
		-- LLVM Linux generation activation
		create {LLVM_CodeGenerator}codeGenerator.init (outputFileName, "x86_64-pc-linux-gnu", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "'LLVM - x86_64-pc-linux-gnu' generation failed to start")
		
		-- MSIL generation activation
		create {MSIL_CodeGenerator}codeGenerator.init (outputFileName + ".msil", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "'MSIL' generation failed to start")
		
		-- JVM generation activation
		create {JVM_CodeGenerator}codeGenerator.init (outputFileName + ".class", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "'JVM' generation failed to start")
		
		-- ARK generation activation
		create {ARK_CodeGenerator}codeGenerator.init (outputFileName + ".abc", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "'ARK' generation failed to start")
		
		-- C-code generation activation
		create {C_CodeGenerator}codeGenerator.init (outputFileName + ".c", buildExecutable)
		registerCodeGenerator (codeGenerator, Result, "'C' generation failed to start")
	ensure
		non_void_list_of_code_generators: Result /= Void
	end -- initCodeGenerators
	
	closeCodeGenerators (generators: Array [CodeGenerator]) is
	require
		non_void_list_of_code_generators: generators /= Void
	local
		index: Integer
	do
		from
			index := generators.count			
		until
			index = 0
		loop
			generators.item (index).dispose
			index := index - 1
		end -- loop
	end -- closeCodeGenerators

	generationFailed (cu: CompilationUnitCommon; fileName: String): Boolean is
	local
		generators: Array [CodeGenerator]
		index: Integer
	do
		generators := initCodeGenerators (fileName, true)			
		index := generators.count
		if index > 0 then
			from
			until
				index <= 0
			loop
				if cu.generationFailed (generators.item (index)) then
					Result := True
				end -- if
				index := index - 1
			end -- loop
			closeCodeGenerators (generators)
		else
			-- No generators
			o.putNL ("Consistency error: No code generators available")
			Result := True
		end -- if
	end -- generationFailed

	executable_build_failed (sysDsc: SystemDescriptor; folderName: String): Boolean is
	require
		non_void_system_dsc: sysDsc /= Void
		is_executable: sysDsc.entry /= Void and sysDsc.from_paths = Void
		non_void_folder_name: folderName /= Void
	local
		entryPointName: String
		clusters: Array [ClusterDescriptor]
		rootUnitDsc: UnitDeclarationDescriptor
		rootUnitCU: CompilationUnitUnit
		entryRtnCU: CompilationUnitStandaloneRoutine
		rtnDsc: StandaloneRoutineDescriptor
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
				create rootUnitCU.init (sysDsc)
				if rootUnitCU.UnitIR_Loaded (clusters.item (1).name + fs.separator + IRfolderName + fs.separator + entryPointName + UnitSuffix + "." + ASText, o) then
					rootUnitDsc := rootUnitCU.unitDclDsc.getUnitDeclaration -- root unit and its alias if any were loaded and registered
					if rootUnitDsc.formalGenerics.count > 0 then
						o.putNL ("Error: entry point `" + entryPointName + "` is in fact generic unit `" + rootUnitDsc.fullUnitName + 
							"` and cannot be used as the root unit")
						Result := True
					elseif rootUnitDsc.isVirtual then
						o.putNL ("Error: root unit `" + entryPointName + "` is abstract and cannot be used as the root unit")
						Result := True
					elseif rootUnitDsc.isExtension then
						o.putNL ("Error: root unit `" + entryPointName + "` is unit extension and cannot be used as the root unit")
						Result := True
					elseif rootUnitDsc.hasNoEntryPointInitProcedure then
						o.putNL ("Error: root unit `" + entryPointName + "` has no proper initialization procedure which can be used as the entry point")
						Result := True
					else
						-- Register root in the context
						sysDsc.registerLoadedUnit (rootUnitDsc)
						Result := failedToLoadRequiredTypes (sysDsc, rootUnitDsc.typePool)
						if not Result then
							if sysDsc.allUnitInterfacesAreValid (o) then
								debug
									sysDsc.dumpContext (o)						
								end -- debug
								-- We need to start with the root unit implementation validate and generate code
								-- and then do the same for its constructor actual usage 
								Result := rootUnitDsc.is_invalid (rootUnitCU, o)
								if not Result then
									Result := generationFailed (rootUnitCU, "_$" + sysDsc.name + fs.separator + sysDsc.name)
									if not Result then
										not_implemented_yet ("Building executable `" + sysDsc.name + "` from unit `" + sysDsc.entry + "` from cluster `" + clusters.item (1).name + "`%N")
										-- 6.Check validity and code generation for used CUs
										-- 7. Build executable
									end -- if
								end -- if
							else
								Result := True
							end -- if
						end -- if
					end -- if
				else
					o.putNL ("Error: root unit `" + entryPointName + "` was not loaded correctly")
					Result := True
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
				o.putLine ("Loading entry point routine `" + entryPointName + "`")
				entryRtnCU := sysDsc.loadStandAloneRoutineFrom (clusters.item (1).name, entryPointName, o)
				if entryRtnCU = Void then
					-- There was a problem to load entry point routine
					o.putNL ("Error: entry point routine `" + entryPointName + "` was not loaded correctly")
					Result := True
				else
					rtnDsc := entryRtnCU.routine
					check
						routine_not_void: rtnDsc /= Void
						routine_generics_not_void: rtnDsc.generics /= Void
					end -- check
					if rtnDsc.generics.count > 0 then
						o.putNL ("Error: entry point `" + entryPointName + "` is in fact generic rotuine `" + entryRtnCU.routine.namefullRoutineName + 
							"` and cannot be used as the entry point")
						Result := True
					elseif rtnDsc.type /= Void then
						o.putNL ("Error: entry point `" + entryPointName + "` is in fact a function, only procedures can be used as the entry point")
						Result := True
					else
						entryRtnCU.attachSystemDescription (sysDsc)
						Result := failedToLoadRequiredTypes (sysDsc, entryRtnCU.typePool)
						if not Result then
							if sysDsc.allUnitInterfacesAreValid (o) then
								debug
									sysDsc.dumpContext (o)						
								end -- debug
								Result := entryRtnCU.routine.is_invalid (entryRtnCU, o)
								if not Result then
									Result := generationFailed (entryRtnCU, "_$" + sysDsc.name + fs.separator + sysDsc.name)
									if not Result then
										not_implemented_yet ("Building executable `" + sysDsc.name + "` from standalone procedure `" + sysDsc.entry + "` from cluster `" + clusters.item (1).name + "`%N")
										-- 6.Check validity and code generation for used CUs
										-- 7. Build executable
									end -- if
								end -- if
							else
								Result := True
							end -- if
						end -- if
					end -- if
				end -- if
			end -- if
		end -- if
	end -- executable_build_failed
	
	failedToLoadRequiredTypes (sysDsc: SystemDescriptor; typesPool: Sorted_Array[TypeDescriptor]): Boolean is
	local		
		typeDsc: TypeDescriptor
		aliasTypes: Sorted_Array [AliasedTypeDescriptor]
		attachedTypeDsc: AttachedTypeDescriptor
		unitDclDsc: UnitDeclarationDescriptor
		i, n: Integer
		unitAliasDsc,
		registeredAliasDsc: UnitAliasDescriptor
		cntTypDsc: ContextTypeDescriptor		
		aliasesCount: Integer
		failedToLoadCount: Integer
	do
		if typesPool /= Void then
			from
				create aliasTypes.make
				n := typesPool.count
				i := 1
			until
				i > n					
			loop
				typeDsc := typesPool.item(i) 
				debug
					--o.putNL ("%T!!! Checking if type `" + typeDsc.out + "` was loaded")
				end -- debug
				if typeDsc.isNotLoaded (sysDsc, o) then
					debug
						--o.putNL ("<<< Failed to load `" + typeDsc.out + "`")
					end -- debug
					failedToLoadCount := failedToLoadCount + 1
					Result := True
				elseif typeDsc.aliasName /= Void then
					attachedTypeDsc ?= typeDsc
					if attachedTypeDsc /= Void then
						if attachedTypeDsc.unitDeclaration /= Void then
							check
								unit_registered: sysDsc.allUnits.seek (attachedTypeDsc.unitDeclaration) > 0
							end 
							create unitAliasDsc.init (attachedTypeDsc.aliasName, attachedTypeDsc.unitDeclaration)
							registeredAliasDsc ?= sysDsc.allUnits.add_it (unitAliasDsc)
							if registeredAliasDsc /= unitAliasDsc and then registeredAliasDsc.unitDclDsc /= unitAliasDsc.unitDclDsc then
								o.putNL ("Error: at least two aliases refer to the same name `" + typeDsc.aliasName + "` for different types")
								Result := True								
							end -- if				
						--else
						--	debug
						--		o.putNL ("Info: type `" + typeDsc.out + "` has no IR!!!")
						--	end -- debug
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
							o.putNL ("Error: type alias `" + unitAliasDsc.aliasName + "` clashes with another unit name")
							Result := True
						end -- if
						i := i + 1								
					end -- if				
				end -- loop
			end -- if
			--if Result then
			--	if failedToLoadCount > 0 then
			--		o.putLine ((sysDsc.allUnits.count - aliasesCount).out + " units loaded, while " + failedToLoadCount.out + " failed to be loaded")
			--		o.newLine
			--	end -- if
			--else
			--	o.putLine ((sysDsc.allUnits.count - aliasesCount).out + " units loaded")
			--	o.newLine
			--end -- if
		end -- if		
	end -- failedToLoadRequiredTypes
	
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
			if sysDsc.allUnitInterfacesAreValid (o) then
				debug
					sysDsc.dumpContext (o)						
				end -- debug
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
				if not Result then
					not_implemented_yet ("Building library `" + sysDsc.name + "` from folder `" + folderName + "`%N")
				end -- if
			else
				Result := True
			end -- if
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
		unitAliasDsc: UnitAliasDescriptor
		index: Integer
	do
		ir_path := path + fs.separator + IRfolderName
		if fs.folderExists (ir_path) then
			debug
				--trace ("Loading interfaces from `" + ir_path + "`")
			end -- debug
			from
				ast_files := fs.getAllFilesWithExtension (ir_path, INText) -- Get all interface IR files
				index := ast_files.count
			until
				index = 0
			loop
				fileDsc := ast_files.item (index)
				fileName := fileDsc.name
				if fileName.has_substring (UnitSuffix) then
					if fileName.has_substring (AliasPrefix) then
					else
						-- That is unit but not alias IR file
						create cuUnitDsc.init (sysDsc)
						if cuUnitDsc.UnitIR_Loaded (fileDsc.path, o) then
							debug
								--trace ("Unit `" + cuUnitDsc.unitDclDsc.fullUnitName + "` loaded from file `" + ast_files.item (index).path + "`")
							end -- debug
							-- Register unit in the context
							sysDsc.registerLoadedUnit (cuUnitDsc.unitDclDsc)
							
							if cuUnitDsc.unitDclDsc.aliasName /= Void then				
								-- We need to process alias as well !!! To have it registered !!!  XXX
								create unitAliasDsc.init (cuUnitDsc.unitDclDsc.aliasName, cuUnitDsc.unitDclDsc)
								unitAliasDsc ?= sysDsc.allUnits.add_it (unitAliasDsc)
								check
									aliad_registered: unitAliasDsc /= Void
								end -- check
							end -- if
							
							-- Load all types it uses
							if failedToLoadRequiredTypes (sysDsc, cuUnitDsc.typePool) then
								Result :=  True
							end -- if
						else
							Result := True
						end -- if				
					end -- if
				elseif fileName.has_substring (RoutinesSuffix) then 
					-- That is standalone routine IR file
					create rtnDsc.init (sysDsc)
					if rtnDsc.RoutineIR_Loaded (fileDsc.path, o) then
						debug
							--trace ("Standalone routine `" + rtnDsc.routine.name + "` loaded from file `" + ast_files.item (index).path + "`")
						end -- debug
						-- Load all types routine uses
						if failedToLoadRequiredTypes (sysDsc, rtnDsc.typePool) then
							Result := True
						end -- if
					else
						Result := True
					end -- if
				end -- if
				index := index - 1
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
				ast_files := fs.getAllFilesWithExtension (ir_path, ASText) -- Get IR implementation files
				i := ast_files.count
			until
				i = 0
			loop
				fileDsc := ast_files.item (i) 
				fileName := fileDsc.name
				if fileName.has_substring (UnitSuffix) and then not fileName.has_substring (AliasPrefix) then
					create cuUnitDsc.init (sysDsc)
					if cuUnitDsc.UnitIR_Loaded (fileDsc.path, o) then
						debug
							--trace ("Type `" + unitDsc.type.fullUnitName + "` loaded from file `" + ast_files.item (i).path + "`")
						end -- debug						
						if cuUnitDsc.unitDclDsc.isInvalid (cuUnitDsc, o) then
							Result := True
						else
							from
								j := generators.count
							until
								j = 0
							loop
								if cuUnitDsc.unitDclDsc.generationFailed(generators.item (j)) then
									Result := True
								end -- if
								j := j - 1
							end -- loop
						end -- if
					else
						Result := True
					end -- if				
				elseif fileName.has_substring (RoutinesSuffix) then 
					create rtnDsc.init (sysDsc)
					if rtnDsc.RoutineIR_Loaded (fileDsc.path, o) then
						debug
							--trace ("Standalone routine loaded from file `" + ast_files.item (i).path + "`")
						end -- debug
						if rtnDsc.routine.isInvalid (rtnDsc, o) then
							Result := True
						else
							from
								j := generators.count
							until
								j = 0
							loop
								if rtnDsc.routine.generationFailed(generators.item (j)) then
									Result := True
								end -- if									
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
	debug
		o.putNL ("NOT IMPLEMENTED YET <" + featureName + ">")
	end
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