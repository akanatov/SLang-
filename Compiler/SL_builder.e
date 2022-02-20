class SLang_builder
inherit
	SLangConstants
create
	init
feature {None}
	env: Environment is
	once
		create Result
	end -- env
	getAnonymousRoutineClusters: Sorted_Array [String] is
	local	
		libPath: String
		start, stop: Integer
	do
		create Result.make
		Result.add (".") -- current folder is always used to look for units
		-- Add what should be added from the environemnt ...
		libPath := env.env_item (SLNG_LIB)
		if libpath /= Void then
			from
				start := 1
				stop := libPath.index_of (';', start) 
			until
				stop = 0
			loop
				Result.add (libPath.substring (start, stop - 1))
				start := stop + 1
				stop := libPath.index_of (';', start) 
			end -- loop
			if start = 1 then
				Result.add (libPath)
			end -- if
		end -- if
	end -- getAnonymousRoutineClusters
feature {Any}
	--scanner: SLang_scanner
	--parser: SLang_parser
	--semAnal: SLang_semAnal
	--cuDsc : CompilationUnitCommon
	build_from_file (fName: String; fs: FileSystem) is
	require
		non_void_file_name: fName /= Void
		non_void_file_system: fs /= Void
	local 
		cuDsc : CompilationUnitAnonymousRoutine
		sysDsc: SystemDescriptor
		--folderName: String
		fileName: String
		codeGenerator: CodeGenerator
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		-- stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]
		statements: Array [StatementDescriptor]
		stmtDsc: StatementDescriptor
		generators: Array [CodeGenerator]
		skipCodeGen: Boolean
		i, n: Integer
		j, m: Integer
	do
		if fs.folderExists (IRfolderName) then
			-- Build the system			
			create cuDsc.init (Void)
			fileName := IRfolderName + "\_" + fs.getFileName(fName) + PgmSuffix + ASText
			if cuDsc.AnonymousRoutineIR_Loaded (fileName, o) then
				o.putNL ("Building a program from file `" + fName + "`")
				-- 1. How to get system description - where to look for units !!! 
				--create sysDsc.init_program (n,e : String; c: like clusters; l: like libraries) is
				create sysDsc.init_program (fs.getFileName(fName), "*", getAnonymousRoutineClusters, Void)
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
					n := typePool.count
					i := 1
				until
					i > n					
				loop
--					debug
--						print ("Load interface of '" + typePool.item(i).getExternalName + "'%N")
--					end -- debug
					if typePool.item(i).isNotLoaded (cuDsc) then
						skipCodeGen := True
					end -- if
					i := i + 1
				end -- loop

				-- 3. Check validity of cuDsc.statements
				from
					statements := cuDsc.statements
					n := statements.count
					i := 1
				until
					i > n
				loop
					if statements.item(i).isInvalid (cuDsc) then
						skipCodeGen := True
					end -- if
					i := i + 1
				end -- loop
				if not skipCodeGen then
					-- 4. Generate code for cudsc.statements

					create generators.make (1, 0)
					
					-- LLVM Windows generation activation
					create {LLVM_CodeGenerator}codeGenerator.init (IRfolderName + "\_" + fs.getFileName(fName), "x86_64-pc-windows-msvc", true)
					registerCodeGenerator (codeGenerator, generators, "Generation 'LLVM - x86_64-pc-windows-msvc' failed to start")
					-- LLVM Linux generation activation
					create {LLVM_CodeGenerator}codeGenerator.init (IRfolderName + "\_" + fs.getFileName(fName), "x86_64-pc-linux-gnu", true)
					registerCodeGenerator (codeGenerator, generators, "Generation 'LLVM - x86_64-pc-linux-gnu' failed to start")
					-- MSIL generation activation
					create {MSIL_CodeGenerator}codeGenerator.init (IRfolderName + "\_" + fs.getFileName(fName), true)
					registerCodeGenerator (codeGenerator, generators, "Generation 'MSIL' failed to start")
					-- JVM generation activation
					create {JVM_CodeGenerator}codeGenerator.init (IRfolderName + "\_" + fs.getFileName(fName), true)
					registerCodeGenerator (codeGenerator, generators, "Generation 'JVM' failed to start")
					-- ARK generation activation
					create {ARK_CodeGenerator}codeGenerator.init (IRfolderName + "\_" + fs.getFileName(fName), true)
					registerCodeGenerator (codeGenerator, generators, "Generation 'ARK' failed to start")
					
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
						from
							j := 1
						until
							j > m
						loop
							generators.item (j).dispose
							j := j + 1
						end -- loop
						-- 5. Link !!!! How ????
					else
						-- No generators
						o.putNL ("Consistency error: No code generators available")
					end -- if
				end -- if
			-- Allready printed!
			--else
			--	-- AST not loaded !!!
			--	o.putNL ("Error: unable to load compiled module from file `" + fileName + "`")
			end -- if
		else
			o.putNL ("Error: SLang folder with artefacts '" + IRfolderName + "' not found")
		end -- if
	end -- build_from_file
	build (sysDsc: SystemDescriptor; fs: FileSystem) is
	require
		non_void_sd: sysDsc /= Void
		non_void_file_system: fs /= Void
	local 
		folderName: String -- name of the folder where object files be stored
	do
		folderName := "_$"
		if sysDsc.name.is_equal ("*") then
			folderName.append_string ("IR")
		else
			folderName.append_string ("$" + sysDsc.name)
		end -- if
		if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
			-- Build the system
			if sysDsc.entry = Void then
				-- Library - all units and routines of the current folder are to be put into the libary
				o.putNL ("Building library `" + sysDsc.name + "`")
not_implemented_yet ("Building library `" + sysDsc.name + "`")
			elseif sysDsc.name.is_equal ("*") then
				-- Set of object files from all units and routines of the current folder
				o.putNL ("Building all files")
not_implemented_yet ("Building all files")
			else
				-- Executable with the entry point which is a standalone routine
				o.putNL ("Building executable `" + sysDsc.name + "`")
not_implemented_yet ("Building executable `" + sysDsc.name + "`")
			end -- if
		else
			o.putNL ("Error: Not able to create SLang folder with artefacts  '" + folderName + "'")
		end -- if
	end -- build
feature {None}
trace (message: String ) is
do
	o.putNL ("Trace: " + message)
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