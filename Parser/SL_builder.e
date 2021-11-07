class SLang_builder
--create
--	init
feature {Any}
	--scanner: SLang_scanner
	--parser: SLang_parser
	--semAnal: SLang_semAnal
	--cuDsc : CompilationUnitCommon
	--o: Output
	--init(anOutput: like o) is
	--do
	--	o := anOutput
	--end -- init
	build_from_file (fName: String; fs: FileSystem; o: Output) is
	require
		non_void_file_name: fName /= Void
		non_void_file_system: fs /= Void
	local 
		cufDsc : CompilationUnitFile
		folderName: String
		cg1, cg2, cg3: CodeGenerator
		statements: Array [StatementDescriptor]
		skipCodeGen: Boolean
		i, n: Integer
	do
		folderName := "_$_IR"
		if fs.folderExists (folderName) then
			-- Build the system
			create {CompilationUnitFile} cufDsc.init
			if cufDsc.FileLoaded (folderName + "\_" + fs.getFileName(fName) + ".Slang#ast", o) then
				o.putNL ("Building a program from file `" + fName + "`")
				--cuDsc := cufDsc
				-- Work with cuDsc further!
				-- 1. Check validity of cuDsc.statements
				-- 2. Generate code for cudsc.statements
				from
					statements := cufDsc.statements
					n := statements.count
					i := 1
				until
					i > n
				loop
					if statements.item(i).isNotValid (cufDsc) then
						skipCodeGen := True
					end -- if
					i := i + 1
				end -- loop
				if not skipCodeGen then
					create {LLVM_Windows_CodeGenerator}cg1.init (folderName + "\_" + fs.getFileName(fName) + "_win.ll")
					create {LLVM_Linux_CodeGenerator}cg2.init (folderName + "\_" + fs.getFileName(fName) + "_lin.ll")
					create {MSIL_CodeGenerator}cg3.init (folderName + "\_" + fs.getFileName(fName) + ".msil")
					from
						statements := cufDsc.statements
						n := statements.count
						i := 1
					until
						i > n
					loop
						statements.item(i).generate_llvm_windows (cg1)
						statements.item(i).generate_llvm_linux (cg2)
						statements.item(i).generate_msil (cg3)
						i := i + 1
					end -- loop
					cg1.dispose
					cg2.dispose
					cg3.dispose
				end -- if
			end -- if
		else
			o.putNL ("Error: SLang folder with artefacts '" + folderName + "' not found")
		end -- if
	end -- build_from_file
	build (sysDsc: SystemDescriptor; fs: FileSystem; o: Output) is
	require
		non_void_sd: sysDsc /= Void
		non_void_file_system: fs /= Void
	local 
		folderName: String -- name of the folder where object files be stored
	do
		folderName := "_$_"
		if sysDsc.name.is_equal ("*") then
			folderName.append_string ("IR")
		else
			folderName.append_string (sysDsc.name)
		end -- if
		if fs.folderExists (folderName) or else fs.folderCreated (folderName) then
			-- Build the system
			if sysDsc.entry = Void then
				-- Library - all units and routines of the current folder are to be put into the libary
				o.putNL ("Building library `" + sysDsc.name + "`")
			elseif sysDsc.name.is_equal ("*") then
				-- Set of object files from all units and routines of the current folder
				o.putNL ("Building all files")
			else
				-- Executable with the entry point which is standalone routine
				o.putNL ("Building executable `" + sysDsc.name + "`")
			end -- if
		else
			o.putNL ("Error: Not able to create SLang folder with artefacts  '" + folderName + "'")
		end -- if
	end -- build
end -- class SLang_builder