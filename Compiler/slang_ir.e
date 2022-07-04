deferred class Identation
inherit
	Any
		undefine 
			out
	end
feature {None}
	ident: Integer_Ref is
	once
		create Result
	end -- ident
	incIdent is
	do
		ident.set_item (ident.item + 1)
	end -- incIdent
	decIdent is
	do
		ident.set_item (ident.item - 1)
	end -- decIdent
	getIdent: String is
	local
		i: Integer
	do
		from
			Result := ""
			i := 1
		until
			i > ident.item
		loop
			Result.append_character ('%T')
			i := i + 1
		end -- loop
	end -- getIdent
end -- class Identation

class SystemDescriptor
-- Context: system (Identifier| StringConstant) 
-- [init Identifier]
-- [use {(Identifier| StringConstant) [":" Options end]} end]
-- [foreign {(Identifier| StringConstant)} end]
-- end
inherit
	Comparable
		redefine
			out, is_equal
	end
	SourcePosition
		redefine
			out, is_equal
	end
	SLangConstants
		redefine
			out, is_equal
	end
	Server
		redefine
			out, is_equal
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init_program, init_library, init_script
feature {Any}
	--name: String -- Name of the output target
	from_paths: Sorted_Array [String] -- List of paths to build the library from
	entry: String -- Name of class or routine to start execution of the program

	clusters: Sorted_Array [ClusterDescriptor] -- Clusters to search for usage of units and routines
	libraries: Sorted_Array [String] -- object/lib/dll imp files to link with the system

	checkSources(o: Output) is
	local
		processedFolders: Sorted_Array [String]
		folderName: String
		i, n: Integer
	do
		if from_paths = Void then
			from
				i := 1
				n := clusters.count
			until
				i > n
			loop
				parseFolder (clusters.item (i).name, o)
				i := i + 1
			end -- loop
		else
			from
				create processedFolders.make
				i := 1
				n := from_paths.count
			until
				i > n
			loop
				folderName := fs.absolute_path (from_paths.item (i))
				parseFolder (folderName, o)
				processedFolders.add (folderName)
				i := i + 1
			end -- loop
			from
				i := 1
				n := clusters.count
			until
				i > n
			loop
				folderName := fs.absolute_path (clusters.item (i).name)
				if processedFolders.search (folderName) = Void then
					parseFolder (folderName, o)
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- checkSources

	safe_file_list (path: String): Array [FSys_Dat] is
	require	
		non_void_path: path /= Void
	local
		wasError: Boolean
	do
		if wasError then
			Result := <<>>
		else
			Result := fs.file_list (path)
			if Result = Void then
				Result := <<>>
			end -- if
		end -- if
	ensure
		non_void_result: Result /= Void
	rescue
		wasError := True
		retry
	end -- safe_file_list
	
	readMarkers (path: String): Sorted_Array [FilePair] is
	local
		files: Array [FSys_Dat]
		fileDsc: FilePair
		fName: String
		ext: String
		i, n: Integer
	do
		from
			create Result.make
			files := safe_file_list (path)
			i := 1
			n := files.count
		until 
			i > n
		loop
			fName := files.item (i).name
			ext := fs.getFileExtension (fName)
			if not (ext.is_equal (ASText) or else ext.is_equal (INText)) and then ext.is_real then
				create fileDsc.init(fs.getFileName(fName), ext)
				Result.add (fileDsc)
			end -- if
			i := i + 1
		end -- loop
	end -- readMarkers

	parseFolder (path: String; o: Output) is
	require	
		non_void_path: path /= Void
		non_void_output: o /= Void
	local
		scanner: SLang_scanner
		parser: SLang_parser
		files: Array [FSys_Dat]
		markerFiles: Sorted_Array [FilePair]
		markerCount: Integer
		fileDsc: FilePair
		srcFileTime: Real
		tagFileTime: Real
		fName: String
		sName: String		
		tsfName: String
		ext: String
		slangFileCount: Integer
		saveErrCount: Integer
		skipBuild: Boolean
		toParse: Boolean
		actualFilesCount: Integer
		i, n: Integer
	do
		o.putLine ("Checking cluser at `" + path + "`")
		from
			files := safe_file_list (path)
			markerFiles := readMarkers (path + fs.separator + IRfolderName)
			markerCount := markerFiles.count
			i := 1
			n := files.count
		until 
			i > n
		loop
			fName := files.item (i).name
			ext := fs.getFileExtension (fName)
			if ext.is_equal (SLangExt) or else ext.is_equal (CLangExt) then
				if markerCount > 0 then
					sName := fs.getFileName(fName)
					srcFileTime := files.item (i).time
					create fileDsc.init(sName, ext)
					fileDsc := markerFiles.search (fileDsc)
					if fileDsc /= Void then
						tagFileTime := fileDsc.fileExtension.to_real
						srcFileTime := files.item (i).time
						toParse := srcFileTime /= tagFileTime
					else
						toParse := True
					end -- if				
				else
					toParse := True
				end -- if
				if toParse then
					slangFileCount := slangFileCount + 1
					if slangFileCount < i then
						files.put (files.item (i), slangFileCount)
					end -- if
				else
					actualFilesCount := actualFilesCount + 1
				--if markerCount > 0 then  -- File was never parsed or not changed since last succsecful parsing 
				--	-- No need to process
				--	o.putLine ("File `" + fName + "` was not changed. Procesing skipped")
				end -- if
			end -- if
			i := i + 1
		end	-- loop
		if slangFileCount < n then
			files.resize (1, slangFileCount)
		end -- if
		inspect
			actualFilesCount
		when 0 then
		when 1 then
			o.putLine ("File is actual ...")
		else
			o.putLine (actualFilesCount.out + " files are actual ...")
		end -- inspect
		n := slangFileCount
		from 
			if n > 0 then
				o.putLine (n.out + " files to parse to actualize ...")
			end -- if
			i := 1
		until
			i > n
		loop
			fName := files.item (i).path
			create scanner.init (fName)
			if scanner.isReady then
				if fs.getFileExtension (fName).is_equal (CLangExt) then
					scanner.setCmode
				else
					scanner.setPmode
				end -- if
				create parser.init (scanner, Void, o)
				o.putLine ("Parsing file `" + fName + "`")
				parser.parseSourceFile
				scanner.close
				parser.ast.attach_usage_pool_to_units_and_standalone_routines
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
							o.putLine ("File `" + fName + "` parsed with no errors!")
						else
							o.putLine (
								"File `" + fName + 
								"` parsed with no errors! But some parsing results were not stored due to " + 
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
			i := i + 1
		end -- loop
	end -- parseFolder
		
	--is_script: Boolean is
	--do
	--	Result := entry /= Void and then entry.is_equal ("*")
	--end -- is_script
	
	init_script (n : String; c: like clusters; l: like libraries) is 
	do
		init_program (n, "*", c, l)
	end -- init_script
	
	init_program (n,e : String; c: like clusters; l: like libraries) is
	require
		name_not_void: n /= Void
		entry_not_void: e /= Void
	do
		name:= n
		entry:= e
		set_clusters_and_libraries (c, l)
	end -- init_program
	
	init_library (n : String; fp: like from_paths; c: like clusters; l: like libraries) is
	require
		name_not_void: n /= Void
	do
		name:= n
		entry := Void
		if fp = Void then
			create from_paths.make
		else
			from_paths := fp
		end -- if
		set_clusters_and_libraries (c, l)
	end -- init_program

	set_clusters_and_libraries (c: like clusters; l: like libraries) is
	do
		if c = Void then
			create clusters.make
		else
			clusters:= c
		end -- if
		if l = Void then
			create libraries.make
		else
			libraries:= l
		end -- if
	end -- set_clusters_and_libraries
	
	is_equal (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		Result := name < other.name
	end -- infix "<"
	out: String is
	local
		i, n: Integer
	do
		Result := "build " + name
		if entry /= Void then
			Result.append_string (" entry " + entry + "%N")
		else
			check
				invariant_check: from_paths /= Void
			end -- check
			n := from_paths.count
			if n > 0 then 
				from
					Result.append_string (" from ")
					i := 1
				until
					i > n
				loop
					Result.append_character (' ')
					Result.append_string (from_paths.item (i))
					i := i + 1
				end -- loop
			end -- if
			Result.append_character ('%N')
		end -- if
		n := clusters.count
		if n > 0 then
			from
				i := 1
				Result.append_string ("%Tcluster")
			until
				i > n
			loop
				Result.append_character (' ')
				Result.append_string (clusters.item(i).out)
				i := i + 1
			end -- loop
			Result.append_character ('%N')
		end -- if
		n := libraries.count
		if n > 0 then
			from
				i := 1
				Result.append_string ("%Tforeign")
			until
				i > n
			loop
				Result.append_character (' ')
				Result.append_string (libraries.item(i))
				i := i + 1
			end -- loop
			Result.append_character ('%N')
		end -- if		
		Result.append_string ("end%N")
	end -- out

	clusterHasRoutine (clusterDsc: ClusterDescriptor; rtnName: String): Boolean is
	require
		non_void_routine_name: rtnName /= Void
		non_void_cluster_dsc: clusterDsc /= Void
	local	
		path: String
		fileName: String
	do
		if not clusterDsc.unitOrRoutineExcluded (rtnName) then
			path := clusterDsc.name
			if path.item (path.count) = '\' or else path.item (path.count) = '/' then
				fileName := clone(path)
			else
				fileName := path + fs.separator
			end -- if
			fileName.append_string (IRfolderName + fs.separator + clusterDsc.getNameAfterRenaming (rtnName) + RoutinesSuffix + "." + INText)
			Result := fs.file_exists (fileName)
		end -- if
	end -- clusterHasRoutine

	clusterHasUnit (clusterDsc: ClusterDescriptor; unitName: String): Boolean is
	require
		non_void_unit_name: unitName /= Void
		non_void_cluster_dsc: clusterDsc /= Void
	local	
		path: String
		fileName: String
	do
		if not clusterDsc.unitOrRoutineExcluded (unitName) then
			path := clusterDsc.name
			if path.item (path.count) = '\' or else path.item (path.count) = '/' then
				fileName := clone(path)
			else
				fileName := path + fs.separator
			end -- if
			fileName.append_string (IRfolderName  + fs.separator + clusterDsc.getNameAfterRenaming (unitName) + UnitSuffix + "." + INText)
			Result := fs.file_exists (fileName)
		end -- if
debug
	if Result then
		--print ("Unit `" + unitName + "` found,  file `" + fileName + "` is in place%N")
	else
		--print ("Unit `" + unitName + "` not found,  file `" + fileName + "` is missed%N")
	end -- if
end -- debug
	end -- clusterHasUnit
	
	hasStandAloneRoutine(rtnName: String): Array [ClusterDescriptor] is
		-- returns list of clusters where such routine exists
	require
		non_void_routine_name: rtnName /= Void
	local
		i, n: Integer
		clusterDsc: ClusterDescriptor
		selected: Array [Integer]
		temp: Array [ClusterDescriptor]
	do
		if clusters /= Void then
			from
				n := clusters.count
				i := 1
				create Result.make (1, 0)
				create selected.make (1, 0)
			until
				i > n
			loop
				clusterDsc := clusters.item (i)
				if clusterHasRoutine (clusterDsc, rtnName) then
					Result.force (clusterDsc, Result.count + 1)
					if clusterDsc.unitOrRoutineSelected (rtnName) then
						selected.force (Result.count, selected.count + 1)
					end -- if
				end -- if
				i := i + 1
			end -- loop
			n := selected.count 
			inspect
				n
			when 0 then
				-- no selected - just return Result
			when 1 then
				-- Oh! There is only one cluster !!!
				Result := <<Result.item (selected.item (1))>>
			else 
				-- we have several (n) clusters having rtnName selected - error situation
				create temp.make (1, n)
				from
					i := 1
				until
					i > n
				loop
					temp.put (Result.item (selected.item (i)), i)
					i := i + 1
				end -- loop
				Result := temp
			end -- inspect
		end -- if
	end -- hasStandAloneRoutine

	hasUnit(unitName: String): Array [ClusterDescriptor] is
		-- returns list of clusters where such type exists
	require
		non_void_unit_name: unitName /= Void
	local
		i, n: Integer
		clusterDsc: ClusterDescriptor
		selected: Array [Integer]
		temp: Array [ClusterDescriptor]
	do
		if clusters /= Void then
			from
				n := clusters.count
				i := 1
				create Result.make (1, 0)
				create selected.make (1, 0)
			until
				i > n
			loop
				clusterDsc := clusters.item (i)
				if clusterHasUnit (clusterDsc, unitName) then
					Result.force (clusterDsc, Result.count + 1)
					if clusterDsc.unitOrRoutineSelected(unitName) then
						selected.force (Result.count, selected.count + 1)
					end -- if
				end -- if
				i := i + 1
			end -- loop
			n := selected.count 
			inspect
				n
			when 0 then
				-- no selected - just return Result
			when 1 then
				-- Oh! There is only one cluster !!!
				Result := <<Result.item (selected.item (1))>>
			else 
				-- we have several (n) clusters having unitName selected - error situation
				create temp.make (1, n)
				from
					i := 1
				until
					i > n
				loop
					temp.put (Result.item (selected.item (i)), i)
					i := i + 1
				end -- loop
				Result := temp
			end -- inspect
		end -- if
	end -- hasUnit
	
invariant
	--name_not_void: name /= Void
	is_program: entry /= Void implies from_paths = Void
	is_library: entry = Void implies from_paths /= Void
	clusters_not_void: clusters /= Void
	libraries_not_void: libraries /= Void	
end -- class SystemDescriptor

class FilePair
inherit
	Comparable
		redefine
			is_equal 
	end
create
	init
feature
	fileName: String
	fileExtension: String
	init (fn: like fileName; fe: like fileExtension) is
	require
		non_void_file_name: fn /= Void
		non_void_file_extension: fe /= Void
	do
		fileName := fn
		fileExtension :=fe
	end -- init
	is_equal (other: like Current): Boolean is
	do
		Result := fileName.is_equal (other.fileName)
	end -- infix "<"

	infix "<" (other: like Current): Boolean is
	do
		Result := fileName < other.fileName
	end -- infix "<"
invariant
	non_void_file_name: fileName /= Void
	non_void_file_extension: fileExtension /= Void	
end -- class FilePair

class ClusterDescriptor
inherit	
	Comparable
		redefine
			out
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature 
	--name: String
	excluded_list: Sorted_Array [String]
	rename_list: Sorted_Array[RenamePair]
	selected_list: Sorted_Array [String]

	unitOrRoutineExcluded (unitOrRtnName: String): Boolean is
	require
		unit_or_routine_name_not_void: unitOrRtnName /= Void
	do
		Result := excluded_list.seek (unitOrRtnName) > 0
	end -- unitOrRoutineExcluded

	unitOrRoutineSelected (unitOrRtnName: String): Boolean is
	require
		unit_or_routine_name_not_void: unitOrRtnName /= Void
	do
		Result := selected_list.seek (getNameAfterRenaming(unitOrRtnName)) > 0
	end -- unitOrRoutineSelected
	
	getNameAfterRenaming (unitOrRtnName: String): String is
	require
		unit_or_routine_name_not_void: unitOrRtnName /= Void
	local
		rnmPair: RenamePair
	do
		create rnmPair.init (unitOrRtnName, "")
		rnmPair := rename_list.search (rnmPair)
		if rnmPair = Void then
			Result := unitOrRtnName
		else
			Result := rnmPair.newName
		end -- if
	end -- getNameAfterRenaming
	
	out: String is
	local
		i, n, m, k: Integer
		addEnd: Boolean
	do
		Result := clone (name)
		n := excluded_list.count
		m := rename_list.count
		k := selected_list.count
		if n > 0 or else m > 0 or else k > 0 then
			--Result.append_character(':')
			if n > 0 then
				from
					addEnd := True
					Result.append_character('%N')
					Result.append_string("hide")
					i := 1
				until
					i > n
				loop
					Result.append_character(' ')
					Result.append_string (excluded_list.item (i))
					i := i + 1
				end -- loop
				Result.append_character('%N')
			end -- if
			if m > 0 then
				from
					addEnd := True
					Result.append_character('%N')
					Result.append_string("use")
					i := 1
				until
					i > m
				loop
					Result.append_character(' ')
					Result.append_string (rename_list.item (i).oldName)
					Result.append_string (" as ")
					Result.append_string (rename_list.item (i).newName)
					i := i + 1
				end -- loop
				Result.append_character('%N')
			end -- if
			if k > 0 then
				from
					addEnd := True
					Result.append_character('%N')
					Result.append_string("select ")
					i := 1
				until
					i > k
				loop
					Result.append_character(' ')
					Result.append_string (selected_list.item (i))
					i := i + 1
				end -- loop
				Result.append_character('%N')
			end -- if
			if addEnd then
				Result.append_string("%Tend%N")
			end -- if
		end -- if		
	end -- out

	init (aName: like name; el: like excluded_list; rl: like rename_list; sl: like selected_list) is
	do
		name := aName
		if el = Void then
			create excluded_list.make
		else
			excluded_list := el
		end -- if
		if rl = Void then
			create rename_list.make
		else
			rename_list := rl
		end -- if
		if sl = Void then
			create selected_list.make
		else
			selected_list := sl
		end -- if
	end -- init
	infix "<" (other: like Current): Boolean is
	do
		Result := name < other.name
	end -- infix "<"
end -- class ClusterDescriptor
class RenamePair
inherit	
	Comparable
	end
create
	init
feature 
	oldName, newName: String
	init (on, nn: String) is
	require
		oldName_not_void: on /= Void
		newName_not_void: nn /= Void
	do
		oldName := on
		newName := nn
	end -- init
	infix "<" (other: like Current): Boolean is
	do
		Result := oldName < other.oldName
	end -- infix "<"
invariant
	oldName_not_void: oldName /= Void
	newName_not_void: newName /= Void
end -- class RenamePair

--1 Compilation : {CompilationUnitCompound}
     
--deferred
class CompilationUnitCommon
inherit
	SLangConstants
	end
	Server
	end
create {None}
	init_pools
feature {Any}
	-- use const UnitTypeName {"," UnitTypeName}
	useConst: Sorted_Array [UnitTypeNameDescriptor]
	stringPool: Sorted_Array [String]
	typePool: Sorted_Array[TypeDescriptor]
	srcFileName: String
	timeStamp: Integer 

	setUseConst (uc: like useConst) is
	require
		non_void_useConst: uc /= Void
	do
		useConst := uc
	end -- setUseConst
	--IR_Loaded (fileName: String; o: Output): Boolean is
	--require
	--	file_name_not_void:  fileName /= Void
	--	output_not_void: o /= Void
	--deferred
	--end -- IR_Loaded

	start_unit_parsing is
	do
		backup_stringPool := stringPool
		backup_typePool := typePool
		create unit_stringPool.make 
		create unit_typePool.make
		stringPool := unit_stringPool
		typePool := unit_typePool
		check
			parsing_mode: scanner /= Void
		end -- check
		scanner.setPool (stringPool)
	end -- start_unit_parsing
	stop_unit_parsing is
	do
		stringPool:= backup_stringPool
		typePool:= backup_typePool
		check
			parsing_mode: scanner /= Void
		end -- check
		scanner.setPool (stringPool)
	end -- stop_unit_parsing

	start_standalone_routine_parsing is
	do
		backup_stringPool:= stringPool
		backup_typePool:= typePool
		stringPool:= rtn_stringPool
		typePool:= rtn_typePool
		check
			parsing_mode: scanner /= Void
		end -- check
		scanner.setPool (stringPool)
	end -- start_standalone_routine_parsing
	stop_standalone_routine_parsing is
	do
		stringPool:= backup_stringPool
		typePool:= backup_typePool
		check
			parsing_mode: scanner /= Void
		end -- check
		scanner.setPool (stringPool)
	end -- stop_standalone_routine_parsing

	add_name_to_standalone_routines_pool (name: String) is
	require
		non_void_name: name /= Void
	do
		rtn_stringPool.add (name)
	end -- add_name_to_standalone_routines_pool

	attachSystemDescription (sDsc: like sysDsc) is
	do
		sysDsc := sDsc
	end -- attachSystemDescription

	sysDsc: SystemDescriptor

	loadUnitInterafceFrom (path, unitExternalName: String; o: Output): CompilationUnitUnit is
	require
		non_void_unit_name: unitExternalName /= Void
		non_void_path: path /= Void
	local	
		fileName: String
	do
		if path.item (path.count) = '\' or else path.item (path.count) = '/' then
			fileName := path + ""
		else
			fileName := path + fs.separator
		end -- if
		fileName.append_string (IRfolderName  + fs.separator + unitExternalName + UnitSuffix + "." + INText)
		create Result.make
		if not Result.UnitIR_Loaded (fileName, o) then
			Result := Void
		end -- if
	end -- loadUnitInterafceFrom	

	loadUnitInterface (unitExternalName, unitPrintableName: String; o: Output; unitDsc:UnitTypeCommonDescriptor): CompilationUnitUnit is
	require
		non_void_unit_name: unitExternalName /= Void
		non_void_print_name: unitPrintableName /= Void		
	local
		clusters: Array [ClusterDescriptor]
	do
debug
	if unitDsc.aliasName /= Void then
--		o.putLine ("Loading interface of `" + unitPrintableName + "` with alias `" + unitDsc.aliasName + "`")
	end -- if
--	o.putNL ("Loading interface of `" + unitPrintableName + "` called `" + unitExternalName + "`")
end	-- debug
		clusters := sysDsc.hasUnit(unitExternalName)
		if clusters = Void or else clusters.count = 0 then
			-- Such type is not found in the search universe !!!
			o.putNL ("Error: type `" + unitPrintableName + "` is not found in the provided universe")
			-- Let's parse all sources across all clusters!
		elseif clusters.count > 1 then
			-- More than one type is found in the search universe !!!
			o.putNL ("Error: " + clusters.count.out + " versions of type `" + unitPrintableName + "` found in the provided universe. Select only one to be used")
		else
			-- Load it
			o.putLine ("Loading interface of `" + unitPrintableName + "`")
			Result := loadUnitInterafceFrom (clusters.item (1).name, unitExternalName, o)
			if Result = Void then
				-- There was a problem to load type interface 
				o.putNL ("Error: type `" + unitPrintableName + "` was not loaded correctly")
			elseif fs.file_exists(Result.srcFileName) then
				-- Check if the type source file was changed after type IR was created. If necessary run the parser. 
				if fs.file_time(Result.srcFileName).rounded /= Result.timeStamp then
					-- Ensure source file parsed
					Result := fileParsedForUnit (Result.srcFileName, o, unitExternalName, Result)
				end -- if
			else
				o.putNL ("Warning: source file for the type `" + unitPrintableName + "` is no longer in place")
			end -- if
		end -- if
	end -- loadUnitInterface
	
feature {None}
	fileParsedForUnit (fName: String; o: Output; unitExternalName: String; unitOfInterest: CompilationUnitUnit): CompilationUnitUnit is 
	require
		non_void_file_name: fName /= Void
		non_void_unit_of_interest: unitOfInterest /= Void
	local
		aScanner: SLang_scanner
		parser: SLang_parser
		sName: String
		tsfName: String
		saveErrCount: Integer
	do
		create aScanner.init (fName)
		if aScanner.isReady then
			if fs.getFileExtension (fName).is_equal (CLangExt) then
				aScanner.setCmode
			else
				aScanner.setPmode
			end -- if
			create parser.init (aScanner, Void, o)
			o.putLine ("Parsing changed file `" + fName + "`")
			parser.parseSourceFile
			aScanner.close
			parser.ast.attach_usage_pool_to_units_and_standalone_routines 
			inspect 
				parser.errorsCount
			when 0 then
				if fs.folderExists (IRfolderName) or else fs.folderCreated (IRfolderName) then
					sName := fs.getFileName(fName)
					saveErrCount := parser.ast.saveInternalRepresentation (fName, aScanner.timeStamp, sName, ASText, o, Void)
					parser.ast.cutImplementation
					saveErrCount := saveErrCount + parser.ast.saveInternalRepresentation (fName, aScanner.timeStamp, sName, INText, o, unitOfInterest)
					if saveErrCount = 0 then
						-- Remove previous timestamp files and store the latest parsing timestamp !!!
						tsfName := fs.getFilePath(fName) + fs.separator + IRfolderName + fs.separator + fs.getFileName(fName)
						fs.remove_files_with_the_same_name (tsfName)
						tsfName.append_string ("." + aScanner.timeStamp.out)
						fs.add_file (tsfName, "r")
						o.putLine ("Changed file `" + fName + "` parsed with no errors!")
						Result := unitOfInterest -- True
					else
						o.putLine (
							"Changed file `" + fName + 
							"` parsed with no errors! But some parsing results were not stored due to " + 
							saveErrCount.out + " I/O errors!"
						)
					end -- if
				else
					o.putLine (
						"Failed to create folder `" + IRfolderName + 
						"` to store internal files. Parsing results of changed file `" + fName + "` are not saved!"
					)
				end -- if
			when 1 then
				o.putLine ("Changed file `" + fName + "` parsed with 1 error!")
			else
				o.putLine ("Changed file `" + fName + "` parsed with " + parser.errorsCount.out + " errors!")
			end -- inspect
		else
			o.putLine ("Changed file `" + fName + "` not found or cannot be opened for parsing")
		end -- if
	end -- fileParsedForUnit

	unitAnyDsc: UnitTypeNameDescriptor is
	once
		create Result.init ("Any", Void)
	end -- unitAnyDsc
	
	scanner: SLang_Scanner
	init_pools (scn: like scanner) is
	do
		create useConst.make 
		create stringPool.make
		create typePool.fill (<<unitAnyDsc>>)
		
		create rtn_stringPool.make
		create rtn_typePool.make
		scanner := scn
	end -- init_pools

	-- Save script pools
	backup_stringPool: Sorted_Array [String]
	backup_typePool: Sorted_Array[TypeDescriptor]
	
feature {Any}	

	-- Parsing type
	unit_stringPool: Sorted_Array [String]
	unit_typePool: Sorted_Array[TypeDescriptor]

	-- Parsing standalone routine
	rtn_stringPool: Sorted_Array [String]
	rtn_typePool: Sorted_Array[TypeDescriptor]

	fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]
	
	setFGpool (fgt_pool: like fgTypes) is
	do
		fgTypes := fgt_pool
	end -- setFGpool
	
invariant
	non_void_const_usage: useConst /= Void
	non_void_stringPool: stringPool /= Void
	non_void_type_pool: typePool /= Void
end -- class CompilationUnitCommon

class CompilationUnitAnonymousRoutine
inherit
	CompilationUnitCommon
	end
create	
	init
feature {Any}
	statements: Array [StatementDescriptor]
feature {None}
	locals: Sorted_Array [LocalAttrDeclarationDescriptor]
	init (scn: like scanner) is
	do
		init_pools (scn)
		create statements.make (1, 0)
		create locals.make
	end -- init
feature {Any}
	addStatement (stmtDsc: StatementDescriptor) is
	require
		non_void_statement: stmtDsc /= Void
	do
		statements.force (stmtDsc, statements.count + 1)
	end -- addStatement

	addedLocalDeclarationStatement (stmtDsc: LocalAttrDeclarationDescriptor): Boolean is
	require
		non_void_statement: stmtDsc /= Void
	do
		Result := locals.added (stmtDsc)
		if Result then	
			addStatement (stmtDsc)
		end -- if
	end -- addStatement

	AnonymousRoutineIR_Loaded (fileName: String; o: Output): Boolean is
	local
		fImage: AnonymousRoutineImage 
		localDsc: LocalAttrDeclarationDescriptor
		i, n: Integer
	do
		fImage := loadFileIR (fileName, o)
		if fImage /= Void then
			useConst	:= fImage.useConst	
			statements	:= fImage.statements	
			stringPool	:= fImage.stringPool	
			typePool	:= fImage.typePool	
			srcFileName := fImage.srcFileName
			timeStamp	:= fImage.timeStamp
			create locals.make
			-- Fill locals from statements			
			from
				i := 1
				n := statements.count
			until
				i > n
			loop
				localDsc ?= statements.item (i)
				if localDsc /= Void then
					locals.add (localDsc)
				end -- if
				i := i + 1
			end -- loop
			Result := True
		end -- if
	end -- AnonymousRoutineIR_Loaded

feature {None}

	loadFileIR (fileName: String; o: Output): AnonymousRoutineImage is
	require
		non_void_file_name: fileName /= Void
	local
		aFile: File
		wasError: Boolean
	do
		if wasError then
			o.putNL ("Consistency error: unable to load anonymous routine code from file `" + fileName + "`")
			Result := Void
		else
			create Result.init_empty
			create aFile.make_open_read (fileName)
			Result ?= Result.retrieved (aFile)
			aFile.close
			if Result = Void then
				o.putNL ("Consistency error: file `" + fileName + "` does not contain anonymous routine code")
			end -- if
		end -- if
	rescue
		wasError := True
		retry
	end -- loadFileIR
invariant
	non_void_statements: statements /= Void
end -- class CompilationUnitAnonymousRoutine

class CompilationUnitUnit
inherit
	CompilationUnitCommon
	end
create	
	init, make
feature {Any}
	type: UnitDeclarationDescriptor

	init (scn: like scanner) is
	do
		init_pools (scn)
		type := Void -- ????
	end -- init
	
	make is
	do
		init_pools (Void)
		--type := Void -- ????
	end -- make

	--isInvalid: Boolean is
	--do
	--	Result := type.isInvalid(sysDsc)
	--end -- isInvalid

	--failedToGenerate (generators: Array [CodeGenerator]): Boolean is
	--require
	--	non_void_list_of_code_generators: generators /= Void
	--local
	--	j, m: Integer
	--do
	--	from
	--		j := 1
	--	until
	--		j > m
	--	loop
	--		if type.failedToGenerate(generators.item (j)) then
	--			Result := True
	--		end -- if
	--		j := j + 1
	--	end -- loop
	--end -- if
	
	instantiate (generics: Array [TypeOrExpressionDescriptor]) is
	require
		non_void_generics: generics /= Void
		non_empty_generics: generics.count > 0
	do
-- not_implemened_yet - instantiate
	end -- instantiate
	
	sameUnitFill (unitDsc: UnitDeclarationDescriptor) is
	require
		non_void_unitDsc: unitDsc /= Void
	do
		if type.is_equal (unitDsc) then
			type := unitDsc
		end -- if
	end -- sameUnitFill
	
	UnitIR_Loaded (fileName: String; o: Output): Boolean is
	local
		uImage: UnitImage 
	do
		uImage := loadUnitIR (fileName, o)
		if uImage /= Void then
			type 		:= uImage.type
			useConst	:= uImage.useConst	
			stringPool	:= uImage.stringPool	
			typePool	:= uImage.typePool	
			srcFileName := uImage.srcFileName
			timeStamp	:= uImage.timeStamp
			Result := True
		end -- if
	end -- UnitIR_Loaded

feature {None}

	loadUnitIR (fileName: String; o: Output): UnitImage is
	require
		file_name_not_void: fileName /= Void
	local
		aFile: File
		wasError: Boolean
	do
		if wasError then
			o.putNL ("Consistency error: unable to load type code from file `" + fileName + "`")
			Result := Void
		else
			create aFile.make_open_read (fileName)
			create Result.init_empty
			Result ?= Result.retrieved (aFile)
			aFile.close
			if Result = Void then
				o.putNL ("Consistency error: file `" + fileName + "` does not contain type code")
			end -- if
		end -- if
	rescue
		wasError := True
		retry
	end -- loadUnitIR

end -- class CompilationUnitUnit

class CompilationUnitStandaloneRoutine -- s
inherit
	CompilationUnitCommon
	end
create	
	init, make
feature {Any}
	--routines: Sorted_Array [StandaloneRoutineDescriptor]
	routine: StandaloneRoutineDescriptor

	init(scn: like scanner) is
	do
		init_pools (scn)
		--create routines.make
	end -- init

	make is
	do
		init_pools (Void)
		--create routines.make
	end -- init

	RoutineIR_Loaded (fileName: String; o: Output): Boolean is
	local
		--rImage: RoutinesImage 
		rImage: RoutineImage 
	do
		rImage := loadRoutineIR (fileName, o)
		if rImage /= Void then
			routine 	:= rImage.routine
			useConst	:= rImage.useConst	
			stringPool	:= rImage.stringPool	
			typePool	:= rImage.typePool	
			srcFileName := rImage.srcFileName
			timeStamp	:= rImage.timeStamp
			Result := True
		end -- if
	end -- RoutineIR_Loaded

feature {None}	
	
	--loadRoutinesIR (fileName: String; o: Output): RoutinesImage is
	loadRoutineIR (fileName: String; o: Output): RoutineImage is
	require
		file_name_not_void: fileName /= Void
	local
		aFile: File
		wasError: Boolean
	do
		if wasError then
			o.putNL ("Consistency error: unable to load standalone routine code from file `" + fileName + "`")
			Result := Void
		else
			create aFile.make_open_read (fileName)
			create Result.init_empty
			Result ?= Result.retrieved (aFile)
			aFile.close
			if Result = Void then
				o.putNL ("Consistency error: file `" + fileName + "` does not contain standalone routine code")
			end -- if
		end -- if
	rescue
		wasError := True
		retry
	end -- loadRoutineIR

--invariant
--	non_void_routines: routines /= Void
end -- class CompilationUnitStandaloneRoutine


class CompilationUnitCompound
-- CompilationUnitCompound: { UseConstDirective }  {StatementsList|StandaloneRoutine|UnitDeclaration}
inherit
	CompilationUnitAnonymousRoutine
		rename 
			init as anonymous_routine_init
	end
	--CompilationUnitStandaloneRoutine --s
	--	rename
	--		init as standalone_routines_init
	--end
	SLangConstants
	end
create	
	init
feature {Any}

	units: Sorted_Array [UnitDeclarationDescriptor]
	routines: Sorted_Array [StandaloneRoutineDescriptor]
	
	attach_usage_pool_to_units_and_standalone_routines is
	local
		i, n, m: Integer
	do
		m := useConst.count
		if m > 0 then
			n := units.count
			from
				i := 1
			until
				i > n
			loop
				units.item (i).attach_use_pool (useConst)
				i := i + 1
			end -- loop
			from
				i := 1
			until
				i > m
			loop
				rtn_typePool.add (useConst.item (i))
				i := i + 1
			end -- loop			
		end -- if
	end -- attach_usage_pool_to_units_and_standalone_routines

	cutImplementation is
	local
		i, n: Integer
	do
		create statements.make (1, 0)
		create locals.make
		from
			i := 1
			n := units.count
		until
			i > n
		loop
			units.item(i).cutImplementation
			i := i + 1
		end -- loop
		from
			i := 1
			n := routines.count
		until
			i > n
		loop
			routines.item(i).cutImplementation
			i := i + 1
		end -- loop
	end -- cutImplementation
	
	saveInternalRepresentation (
		FullSourceFileName: String; tStamp: Integer; SourceFileName: String; irFileExtension: String; o: Output; unitOfInterest: CompilationUnitUnit
	): Integer is
	require	
		src_file_name_not_void: FullSourceFileName /= Void
		IR_file_name_not_void: SourceFileName /= Void
		ir_file_extenstion_not_void: irFileExtension /= Void
	local
		sImg: AnonymousRoutineImage
		uImg: UnitImage
		rImg: RoutineImage -- s
		i, n: Integer
		fName: String
		filePrefix: String
		unitDsc: UnitDeclarationDescriptor
	do
		filePrefix := fs.getFilePath(FullSourceFileName) + fs.separator + IRfolderName  + fs.separator
		if statements.count > 0 then
			-- Anonymous Routine IR: useConst + statements 
			create sImg.init (FullSourceFileName, tStamp, useConst, statements, stringPool, typePool)
			fName := filePrefix  + "_" + SourceFileName + ScriptSuffix + "." + irFileExtension
			if not IRstored (fName, sImg) then
				o.putNL ("File open/create/write/close error: unable to store anonymous routine IR into file `" + fName + "`")
				Result := Result + 1
			end -- if
		end -- if

		from
			i := routines.lower
			n := routines.upper
		until
			i > n
		loop
			-- Per standalone routine: useConst + routine
			create rImg.init (FullSourceFileName, tStamp, useConst, routines.item (i), rtn_stringPool, rtn_typePool)
			fName := filePrefix  + routines.item (i).name + RoutinesSuffix + "." + irFileExtension
			if not IRstored (fName, rImg) then
				o.putNL ("File open/create/write/close error: unable to store standalone routine IR into file `" + fName + "`")
				Result := Result + 1
			end -- if
			i := i + 1
		end -- loop

		--if routines.count > 0 then
		--	-- Standalone routines: useConst + routines
		--	create rImg.init (FullSourceFileName, tStamp, useConst, routines, rtn_stringPool, rtn_typePool)
		--	fName := filePrefix  + "_" + SourceFileName + RoutinesSuffix + "." + irFileExtension
		--	if not IRstored (fName, rImg) then
		--		o.putNL ("File open/create/write/close error: unable to store standalone routines IR into file `" + fName + "`")
		--		Result := Result + 1
		--	end -- if
		--end -- if

		from
			i := units.lower
			n := units.upper
		until
			i > n
		loop
			-- per type: useConst + type
			unitDsc := units.item(i)
			create uImg.init (FullSourceFileName, tStamp, useConst, unitDsc, unitDsc.stringPool, unitDsc.typePool)
			fName := filePrefix  + unitDsc.getExternalName + UnitSuffix + "." + irFileExtension
			if IRstored (fName, uImg) then
				if unitDsc.aliasName /= Void then
					-- Straightforward decision to store the full copy of IR for the alias name ... May be optimized
					fName := filePrefix  + unitDsc.getAliasExternalName + UnitSuffix + "." + irFileExtension
					if not IRstored (fName, uImg) then
						o.putNL ("File open/create/write/close error: unable to store type IR into file `" + fName + "`")
						Result := Result + 1
					end -- if
				end -- if
				if unitOfInterest /= Void then
					unitOfInterest.sameUnitFill (uImg.type)
				end -- if
			else
				o.putNL ("File open/create/write/close error: unable to store type IR into file `" + fName + "`")
				Result := Result + 1
			end -- if
			i := i + 1
		end -- loop

	end -- saveInternalRepresentation

feature {None}

	IRstored (fileName: String; image: Storable): Boolean is
	require
		non_void_file_name: fileName /= Void
		non_void_image: image /= Void
	local
		aFile: File
		wasError: Boolean
	do	
		if not wasError then
			create aFile.make_create_read_write (fileName) 
			image.independent_store (aFile)
			aFile.close
			Result := True
		end -- if
	rescue
		wasError := True
		retry
	end -- IRstored

	init (scn: like scanner) is
	do
		anonymous_routine_init (scn)
		--standalone_routines_init (scn)
		create routines.make
		create units.make 
	end -- init
invariant
	non_void_units: units /= Void
	non_void_routines: routines /= Void
end -- class CompilationUnitCompound

class IR_Storage
inherit
	Storable
	end
create {None} 
	init_empty
feature
	srcFileName: String
	timeStamp: Integer -- time stamp of the source file 'srcFileName'
	useConst: Sorted_Array [UnitTypeNameDescriptor]
	stringPool: Sorted_Array [String]
	typePool: Sorted_Array[TypeDescriptor]
feature {IR_Storage}
	init_empty is
	do
	end -- init_empty
	init_storage (fn: like srcFileName; ts: like timeStamp; uc: like useConst; sp: like stringPool; tp: like typePool) is
	do
		srcFileName := fn
		timeStamp   := ts
		useConst	:= uc
		stringPool  := sp
		typePool	:= tp
	end -- init_storage
end -- class IR_Storage

class AnonymousRoutineImage
-- local class to store anonymous routine IR
inherit
	IR_Storage
	end
create	
	init, init_empty
feature {CompilationUnitCommon}
	statements: Array [StatementDescriptor]
	init (fn: like srcFileName; ts: like timeStamp; uc: like useConst; stmts: like statements; sp: like stringPool; tp: like typePool) is
	do
		statements	:= stmts
		init_storage (fn, ts, uc, sp, tp)
	end -- init
end -- class AnonymousRoutineImage

class RoutineImage
-- local class to store standalone routine IR
inherit
	IR_Storage
	end
create	
	init, init_empty
feature {CompilationUnitCommon}
	routine: StandaloneRoutineDescriptor
	init (fn: like srcFileName; ts: like timeStamp; uc: like useConst; r: like routine; sp: like stringPool; tp: like typePool) is
	do
		routine	:= r
		init_storage (fn, ts, uc, sp, tp)
	end -- init
end -- class RoutineImage

--class RoutinesImage
---- local class to store standalone routines IR
--inherit
--	IR_Storage
--	end
--create	
--	init, init_empty
--feature {CompilationUnitCommon}
--	routines: Sorted_Array [StandaloneRoutineDescriptor]
--	init (fn: like srcFileName; ts: like timeStamp; uc: like useConst; r: like routines; sp: like stringPool; tp: like typePool) is
--	do
--		routines	:= r
--		init_storage (fn, ts, uc, sp, tp)
--	end -- init
--end -- class RoutinesImage

class UnitImage
-- local class to store type IR
inherit
	IR_Storage
	end
create	
	init, init_empty
feature {CompilationUnitCommon}
	type: UnitDeclarationDescriptor
	init (fn: like srcFileName; ts: like timeStamp; uc: like useConst; u: like type; sp: like stringPool; tp: like typePool) is
	do
		type 	:= u
		init_storage (fn, ts, uc, sp, tp)
	end -- init
end -- class UnitImage

------------ AST/IR classes -------------------------------------------

class WhenClauseDescriptor
-- when [Identifier:] UnitTypeDescriptor do StatementsList
inherit	
	BuildServer
		redefine
			out
	end
	Identation
		undefine
			is_equal
	end
create
	init, make
feature {Any}
	identifier: String
	unitType: UnitTypeCommonDescriptor
	exprDsc: ExpressionDescriptor
	statements: Array [StatementDescriptor]
	
	make (e: like exprDsc; stmts: like statements) is
	require
		non_void_expression: e /= Void	
	do
		exprDsc := e
		if stmts = Void then
			create statements.make (1, 0)
		else
			statements := stmts
		end -- if
	end -- make

	init (id: like identifier; ut: like unitType; stmts: like statements) is
	require
		non_void_unitType: ut /= Void	
	do
		identifier := id
		unitType := ut
		if stmts = Void then
			create statements.make (1, 0)
		else
			statements := stmts
		end -- if
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := "when "
		if exprDsc = Void then
			if identifier /= Void then
				Result.append_string (identifier)
				Result.append_string (": ")
			end -- if
			Result.append_string (unitType.out)
		else
			Result.append_string (exprDsc.out)			
		end -- if		
		Result.append_string (" do%N")
		from
			i := 1
			n := statements.count
			incIdent
		until
			i > n
		loop
			Result.append_string (getIdent + statements.item (i).out)
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			i := i + 1
		end -- loop
		decIdent
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]
		-- notValid: Boolean
		i, n: Integer
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool

--	identifier: String   -- should be valid
		if identifier /= Void then
-- not_implemented_yet
		end -- if
--	unitType: UnitTypeCommonDescriptor -- should be valid
-- not_implemented_yet

--	statements: Array [StatementDescriptor] -- should be valid
		from
			i := 1
			n := statements.count
		until
			i > n
		loop
			if statements.item (i).isInvalid (context, o) then
				Result := True
			end
			i := i + 1
		end -- loop
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	consistent: unitType /= Void or else exprDsc /= Void
	non_void_statements: statements /= Void
end -- class WhenClauseDescriptor

class InnerBlockDescriptor
-- do ["{"Identifier {"," Identifier} "}"]  StatementsList [ WhenClause {WhenClause} [else [StatementsList]] ]
inherit	
	StatementDescriptor
	end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}
	invariantOffList: Sorted_Array [String]
	statements: Array [StatementDescriptor]
	whenClauses: Array [WhenClauseDescriptor]
	whenElseClause: Array [StatementDescriptor]

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		i, n: Integer
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- Check that all names in 'invariantOffList' are valid entities within the block
		from
			i := 1
			n := invariantOffList.count
		until
			i > n
		loop
			i := i + 1
			--if invariantOffList.item (i) then -- is the valid and visible name of the entity
			--	Result := True
			--end -- if
		end -- loop
		-- Check that all 'statements' are valid 
		from
			i := 1
			n := statements.count
		until
			i > n
		loop
			if statements.item(i).isInvalid (context, o) then
				Result := True
			end
			i := i + 1
		end -- loop
		-- Check that all 'whenClauses' are valid
			-- Check that all statements in whenElseClause are valid 		
		n := whenClauses.count
		if n > 0 then
			from
				i := 1
			until
				i > n
			loop
				if whenClauses.item(i).isInvalid (context, o) then
					Result := True
				end
				i := i + 1
			end -- loop
			from
				i := 1
				n := whenElseClause.count
			until
				i > n
			loop
				if whenElseClause.item(i).isInvalid (context, o) then
					Result := True
				end
				i := i + 1
			end -- loop
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	cutImplementation is
	do
		create invariantOffList.make
		create statements.make (1, 0)
		create whenClauses.make (1, 0)
		create whenElseClause.make (1, 0)
	end -- cutImplementation
	
	init (invOff: like invariantOffList; stmts: like statements; wc: like whenClauses; wec: like whenElseClause) is
	do
		if invOff = Void then
			create invariantOffList.make
		else
			invariantOffList:= invOff
		end -- if
		if stmts = Void then
			create statements.make (1, 0)
		else
			statements := stmts
		end -- if
		if wc = Void then
			create whenClauses.make (1, 0)
		else
			whenClauses:= wc
		end -- if
		if wec = Void then
			create whenElseClause.make (1, 0)
		else
			whenElseClause:= wec
		end -- if		
	end -- init
	
	out: String is
	local
		i, n: Integer
	do
		if statements.count = 0 and then whenClauses.count = 0 and then whenElseClause.count = 0 then
			Result := getIdent + "do%N"
		else
			Result := getIdent + "do"			
			-- invariantOffList
			n := invariantOffList.count
			if n > 0 then
				from
					Result.append_string (" {")
					i := 1
				until
					i > n
				loop
					Result.append_string (invariantOffList.item (i).out)
					if i < n then
						Result.append_character (',')
						Result.append_character (' ')
					end -- if
					i := i + 1
				end -- loop
				Result.append_character ('}')
			end -- if			
			Result.append_character('%N')
			
			from
				i := 1
				n := statements.count
				incIdent
			until
				i > n
			loop
				Result.append_string (getIdent + statements.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
			from
				i := 1
				n := whenClauses.count
				--incIdent
			until
				i > n
			loop
				Result.append_string (getIdent + whenClauses.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			--decIdent
			n := whenElseClause.count
			if n > 0 then
				from
					Result.append_string (getIdent + "else%N")
					incIdent
					i := 1
				until
					i > n
				loop
					Result.append_string (getIdent + whenElseClause.item (i).out)
					if Result.item (Result.count) /= '%N' then
						Result.append_character ('%N')
					end -- if
					i := i + 1
				end -- loop
				decIdent
			end -- if
		end -- if
	end -- out
invariant
	non_void_statements: statements /= Void
	non_void_invariantOffList: invariantOffList /= Void
	non_void_whenClauses : whenClauses /= Void
	non_void_whenElseClause: whenElseClause /= Void
end -- class InnerBlockDescriptor

class StandaloneRoutineDescriptor
-- [pure|safe] Identifier [FormalGenerics] [Parameters] [":" Type] [EnclosedUseDirective]
-- [RequireBlock]
-- (InnerBlock [EnsureBlock] BlockEnd)|(((‚Äú=>‚ÄùExpression)|foreign) [EnsureBlock BlockEnd])
inherit
	--Comparable
	--	redefine	
	--		out, is_equal
	--end
	RoutineDescriptor
		redefine
			is_equal
		--	out, is_equal
	end
	NamedDescriptor
		redefine
			is_equal
	end
	--BuildServer
	--	redefine
	--		out, is_equal
	--end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}
	--name: String
	generics: Array [FormalGenericDescriptor]
	fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]
	
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	do
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- checkValidity

	cutImplementation is
	do
		create preconditions.make (1, 0)
		create postconditions.make (1, 0)
		-- clear inner block or "=>" Expression
		if innerBlock /= Void then
			innerBlock.cutImplementation
		end -- if
		expr := Void
	end -- cutImplementation

	instantiate (factualGenerics: Array [TypeOrExpressionDescriptor]) is
	require
		non_void_generics: factualGenerics /= Void
		non_empty_generics: factualGenerics.count > 0
	do
-- not_implemened_yet - instantiate
	end -- instantiate
	
	getExternalName: String is
	local	
		i, n: Integer
	do
		Result := clone (name)
		n := parameters.count
		if n > 0 then
			Result.append_character('$')
			from
				i := 1
			until
				i > n
			loop
				Result.append_character ('_')
				Result.append_string (parameters.item (i).getExternalName)
				i := i + 1
			end
			Result.append_character ('$')
		end -- if		
		if type /= Void then
			Result.append_character ('$')
			Result.append_string (type.getExternalName)
		end -- if
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	out: String is
	local	
		i, n: Integer
	do
		if isPure then
			Result := "pure "
		elseif isSafe then
			Result := "safe "
		else
			Result := ""
		end -- if
		Result.append_string (name)
		Result.append_character(' ')
		
		if generics /= Void then
			n := generics.count
			if n > 0 then
				from
					--Result.append_character(' ')
					Result.append_character('[')
					i := 1
				until
					i > n
				loop
					Result.append_string (generics.item (i).out)
					if i < n then
						Result.append_string (", ")
					end
					i := i + 1
				end -- loop
				Result.append_character (']')
				Result.append_character (' ')
			end -- if
		end -- if
		
		n := parameters.count
		if n > 0 then
			Result.append_character('(')
			from
				i := 1
			until
				i > n
			loop
				Result.append_string (parameters.item (i).out)
				if i < n then
					Result.append_string ("; ")
				end
				i := i + 1
			end
			Result.append_character (')')
		end -- if		
		if type /= Void then
			Result.append_string (": ")
			Result.append_string (type.out)
		end -- if
		if usage /= Void then
			n := usage.count 
			if n > 0 then
				Result.append_string (" use ")	
				from
					i := 1
				until
					i > n
				loop
					Result.append_string (usage.item (i).out)	
					if i < n then
						Result.append_string (", ")
					end
					i := i + 1
				end
				Result.append_character ('%N')
			end -- if
		end -- if
		if isOneLine then
			Result.append_string (" => ")	
			--if expr = Void then
			--	Result.append_string ("<expression>%N")	
			--else 
				Result.append_string (expr.out)	
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if			
			--end -- if
		else
			n := preconditions.count 
			if n > 0 then
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				Result.append_string (getIdent + "require%N")
				incIdent
				from
					i := 1
				until
					i > n
				loop
					Result.append_string (getIdent + preconditions.item (i).out)	
					if Result.item (Result.count) /= '%N' then
						Result.append_character ('%N')
					end -- if
					i := i + 1
				end -- loop
				decIdent
			end -- if
			if isForeign then
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				Result.append_string (getIdent + "foreign%N")	
			elseif innerBlock = Void then
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				Result.append_string (getIdent + "none%N")
			else
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				Result.append_string (innerBlock.out)	
			end -- if
			n := postconditions.count 
			if n > 0 then
				Result.append_string (getIdent + "ensure%N")	
				incIdent
				from
					i := 1
				until
					i > n
				loop
					Result.append_string (getIdent + postconditions.item (i).out)	
					if Result.item (Result.count) /= '%N' then
						Result.append_character ('%N')
					end -- if
					i := i + 1
				end -- loop
				decIdent
			end -- if
			if not (isForeign and then n = 0) then
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				Result.append_string (getIdent +  "end // " + name)	
			end -- if
		end -- if
	end
	infix "<" (other: like Current): Boolean is
	do
		Result := name < other.name -- or else overloading - check types of parameters
	end
	is_equal (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name) -- or else overloading - check types of parameters
	end
	init (
		isP, isS, isF: Boolean; aName: like name; fg: like generics; fgt: like fgTypes;
		params: like parameters; aType: like type; u: like usage; icf: like constants;
		pre: like preconditions; ib: like innerBlock; anExpr: like expr; post: like postconditions
	) is
	require
		name_not_void: aName /= Void
	do
		name := aName
		generics := fg
		fgTypes := fgt
		if params = Void then
			create parameters.make (1, 0)
		else
			parameters := params
		end -- if
		if pre = Void then
			create preconditions.make (1, 0)
		else
			preconditions := pre
		end -- if
		if post = Void then
			create postconditions.make (1, 0)
		else
			postconditions := post
		end -- if
		isPure := isP
		isSafe := isS
		isForeign := isF
		--isOneLine := anExpr /= Void
		type:= aType
		usage := u
		constants := icf
		innerBlock := ib
		expr := anExpr
	end -- init
invariant
	parameters_not_void: parameters /= Void
	name_not_void: name /= Void
	preconditions_not_void: preconditions /= Void
	postconditions_not_void: postconditions /= Void		
end 


deferred class ParameterDescriptor
inherit	
	SmartComparable
		undefine
			out 
	end
	BuildServer
		undefine
			out, is_equal 
	end
	NamedDescriptor
		undefine
			is_equal
	end
feature {Any}
	--name: String
	getExternalName: String	is
	deferred
	ensure
		non_void_external_name: Result /= Void
	end -- getExternalName
feature {None}
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- checkValidity
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
invariant
	--name_not_void: name /= Void
end -- class ParameterDescriptor

class NamedParameterDescriptor
-- Parameter: [[rigid] Identifier ":" Type
inherit	
	ParameterDescriptor
	end
create 
	init
feature {Any}
	isRigid: Boolean
	type: TypeDescriptor
	out: String is
	do
		if isRigid then
			Result := "rigid "
		else
			Result := ""
		end -- if
		Result.append_string (name + ": " + type.out)
	end
	getExternalName: String	is
	do
		Result := type.getExternalName
	end -- getExternalName

	sameAs (other: like Current): Boolean is
	do
--		Result := isRigid = other.isRigid and then type.is_equal (other.type)
		Result := type.is_equal (other.type)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := type < other.type
--		Result := not isRigid and then other.isRigid
--		if not Result and then isRigid = other.isRigid then
--			Result := type < other.type
--		end -- if
	end -- lessThan
	setType (aType: like type) is
	require
		type_not_void: aType /= Void
	do
		type := aType
	end -- setType
feature {None}
	init (iv: Boolean; aName: like name; aType: like type) is
	require
		non_void_name: aName /= Void
		non_void_type: aType /= Void
	do
		isRigid := iv
		name := aName
		type := aType
	end -- init
invariant
	type_not_void: type /= Void
end  -- class NamedParameterDescriptor

class InitialisedParameterDescriptor
-- Parameter: Identifier "is" Expression
inherit	
	ParameterDescriptor
	end
create 
	init
feature {Any}
	expr: ExpressionDescriptor
	out: String is
	do
		Result := name + " is " + expr.out
	end
	getExternalName: String	is
	do
		Result := clone(name)  + "$is"
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	sameAs (other: like Current): Boolean is
	do
-- REDO!!!!
		Result := expr.is_equal (other.expr)
--		Result := expr.type.is_equal (other.expr.type)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := expr < other.expr
--		Result := expr.type < other.expr.type
	end -- lessThan
feature {None}
	init (aName: like name; e: like expr) is
	require
		non_void_name: aName /= Void
		non_void_expression: e /= Void
	do
		name := aName
		expr := e
	end -- init
invariant
	expression_not_void: expr /= Void
	name_not_void: name /= Void
end  -- class InitialisedParameterDescriptor

class AssignAttributeParameterDescriptor
-- Parameter: ":=" [Identifier]
inherit	
	ParameterDescriptor
	end
create 
	init
feature {Any}
	out: String is
	do
		if name.count = 0 then
			Result := ":="
		else
			Result := ":= " + name
		end -- if
	end -- out
	getExternalName: String	is
	do
		Result := out
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := name < other.name
	end -- lessThan
feature {None}
	init (aName: like name) is
	require
		non_void_name: aName /= Void
	do
		name := aName
	end -- init
end  -- class AssignAttributeParameterDescriptor

class EnclosedUseEementDescriptor
-- UnitTypeNameDescriptor [as Identifier]]
inherit	
	Comparable
		redefine
			out, is_equal
	end
create	
	init
feature {Any}
	unitType: UnitTypeNameDescriptor
	newName: String
	out: String is
	do
		Result := unitType.out
		if newName /= Void then
			Result.append_string (" as ")
			Result.append_string (newName)
		end -- if
	end -- out
	infix "<" (other: like Current): Boolean is
	do
		Result := unitType < other.unitType
	end -- infix "<"
	is_equal (other: like Current): Boolean is
	do
		Result := unitType.is_equal (other.unitType) -- or else overloading - check types of parameters
	end -- is_equal
	init (ut: like unitType; name: like newName) is
	require
		non_void_unit_type: ut /= Void
	do
		unitType := ut
		newName := name
	end -- init
invariant
	non_void_unit_type: unitType /= Void
end


-- RequireBlock : require PredicatesList 
-- EnsureBlock  : ensure PredicatesList
-- InvariantBlock: require PredicatesList
-- PredicatesList : [PredicateDescriptor {[";"|","] PredicateDescriptor}] : Array [PredicateDescriptor]

class PredicateDescriptor
-- BooleanExpression [DocumentingComment]
inherit	
	BuildServer
		redefine
			out
	end
create	
	init
feature {Any}
	expr: ExpressionDescriptor
	tag: String
	out: String is
	do
		Result := expr.out
		if tag /= Void then
			Result.append_string (" //")
			Result.append_string (tag)
		end -- if
	end -- out
	init (e: like expr; s: like tag) is
	require
		non_void_predicate: e /= Void
	do
		expr := e
		tag := s
	end -- init
	generate (cg: CodeGenerator) is
	do
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
		if expr.isInvalid (context, o) then
			Result := True
		else -- if type of expr is not Boolean
		end -- if
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- is_invalid

invariant
	non_void_predicate: expr /= Void
end -- class PredicateDescriptor

class UseConstBlock
create
	init
feature {Any}
	usage: Sorted_Array [EnclosedUseEementDescriptor]
	constants: Sorted_Array [UnitTypeNameDescriptor] --FullUnitNameDescriptor]
	init (u: like usage; icf: like constants) is
	require
		consistent: not (u = Void and then icf = Void)
	do
		if u = Void then
			create usage.make
		else
			usage := u
		end -- if
		if icf = Void then
			create constants.make
		else
			constants := icf
		end -- if
	end -- init
invariant
	non_void_usage: usage /= Void
	non_void_importConstantsFrom: constants /= Void
end -- class UseConstBlock

class UnitDeclarationDescriptor
-- UnitDeclaration: ([final] [ref|val|concurrent])|[abstract]|[extend]
-- type Identifier [AliasName] [FormalGenerics] [InheritDirective] [EnclosedUseDirective]
-- [MemberSelection]
-- [InheritedMemberOverriding]
-- [InitProcedureInheritance]
-- [ConstObjectsDeclaration]
-- { ( MemberVisibility ":" {MemberDeclaration}) |  MemberDeclaration }
-- [InvariantBlock]
-- end
inherit
	Comparable
		redefine
			is_equal, out
	end
	BuildServer
		undefine
			out, is_equal
	end
	NamedDescriptor
		undefine
			is_equal
	end
	Identation
		undefine
			is_equal
	end
create 
	init
feature {Any}
	isFinal,
	isRef,
	isVal,
	isConcurrent,
	isVirtual,
	isExtension: Boolean
	--name: String
	aliasName: String
	
	formalGenerics: Array [FormalGenericDescriptor]
		--  "["" FormalGenericDescriptor {"," FormalGenericDescriptor}"]" 
	fgTypes: Sorted_Array [FormalGenericTypeNameDescriptor]
	hasFormalGenericParameter (aName: String): Boolean is
	local
		fgtDsc: FormalGenericTypeNameDescriptor
	do
		if fgTypes /= Void then
			create fgtDsc.init (aName)
			Result := fgTypes.seek (fgtDsc) > 0 
		end -- if
	end -- hasFormalGenericParameter

	getUnitTypeDescriptor: UnitTypeCommonDescriptor is
	do
		create {UnitTypeNameDescriptor}Result.init (name, Void)
		-- Expected generics: Array [TypeOrExpressionDescriptor]
		-- Here generics: 	formalGenerics: Array [FormalGenericDescriptor]
		if formalGenerics /= Void and then formalGenerics.count > 0 then
			print ("%NNOT_IMPLEMENTED_YET: generics!!!%N")
-- not_implemened_yet
		end --if
	end -- getUnitTypeDescriptor
	
	getAliasExternalName: String is
	do
		Result := buildExternalName (aliasName)
	end -- getAliasExternalName
	getExternalName: String is
	do
		Result := buildExternalName (name)
	end -- getExternalName
	buildExternalName(aName: String): String is
	local
		i, n: Integer
	do
		Result := ""
		Result.append_string (aName)
		n := formalGenerics.count
		if n > 0 then
			from
				Result.append_character ('$')
				i := 1
			until
				i > n
			loop
				Result.append_character ('_')
				Result.append_string (formalGenerics.item (i).getExternalName(i))
				--Result.append_string (i.out)
				--Result.append_character ('_')
				--Result.append_string (formalGenerics.item (i).getExternalName)
				--if i < n then
				--	Result.append_character ('_')
				--end
				i := i + 1
			end -- loop
			Result.append_character ('$')
		end -- if
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
		
	parents: Sorted_Array [ParentDescriptor]
		-- extend ParentDescriptor {"," ParentDescriptor}
	
	findParent (utnDsc: UnitTypeNameDescriptor): UnitTypeNameDescriptor is
	require
		unit_descriptor_not_void: utnDsc /= Void
	local
		parDsc: ParentDescriptor
	do
		create parDsc.init (False, utnDsc)
		parDsc := parents.search (parDsc)
		if parDsc /= Void then
			Result := parDsc.parent
		end -- i f
	end -- findParent

	usage: Sorted_Array [EnclosedUseEementDescriptor]
	constants: Sorted_Array [UnitTypeNameDescriptor] --FullUnitNameDescriptor]
		-- EnclosedUseDirective: [use [EnclosedUseEement {"," EnclosedUseEement}] [const FullUnitNameDescriptor {"," FullUnitNameDescriptor}]]

	memberSelections: Sorted_Array [SelectionDescriptor]
		-- select SelectionDescriptor {"," SelectionDescriptor}	
	
	inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]
		-- override InheritedMemberOverridingDescriptor {"," InheritedMemberOverridingDescriptor}
	
	inhertitedInits: Sorted_Array [InitFromParentDescriptor]
		-- new InitFromParentDescriptor {[","] InitFromParentDescriptor}
	
	constObjects: Sorted_Array [ConstObjectDescriptor]
	
	unitMembers: Sorted_Array [MemberDeclarationDescriptor]
	initMembers: Sorted_Array [MemberDeclarationDescriptor]
	
	invariantPredicates: Array [PredicateDescriptor]

	setInvariant (ip: like invariantPredicates) is
	require
		non_void_invariant: ip /= Void
	do
		invariantPredicates := ip
	end -- setInvariant

	setFormalGenercis (fg: like formalGenerics; fgt: like fgTypes) is
	require
		formalGenerics_not_void: fg /= Void
	do
		formalGenerics := fg
		fgTypes := fgt
	end -- setFormalGenercis

	setUseConstBlock (ucb: UseConstBlock) is
	require
		non_void_use_const_block: ucb /= Void
	do
		usage := ucb.usage
		constants := ucb.constants
	end -- setUseConstBlock
	
	cutImplementation is
	local
		i, n: Integer
	do
		from
			n := unitMembers.count
			i := 1
		until
			i > n
		loop
			unitMembers.item (i).cutImplementation
			i := i + 1
		end -- loop
		from
			n := initMembers.count
			i := 1
		until
			i > n
		loop
			initMembers.item (i).cutImplementation
			i := i + 1
		end -- loop
		create invariantPredicates.make (1, 0)
	end -- cutImplementation

	fullUnitName: String is
	local
		i, n: Integer
	do
		Result := ""
		Result.append_string (name)
		n := formalGenerics.count
		if n > 0 then
			from
				Result.append_string (" [")
				i := 1
			until
				i > n
			loop
				Result.append_string (formalGenerics.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
			Result.append_character (']')
		end -- if
	end -- fullUnitName
	
	out: String is
	local
		i, n: Integer
	do
		-- ([final] [ref|val|concurrent])|[abstract]|[extend]
		if isExtension then
			Result := "extend "
		else
			Result := ""
			if isFinal then
				Result.append_string ("final ")
			elseif isVirtual then
				Result.append_string ("abstract ")
			end -- if
			if isRef then
				Result.append_string ("ref ")
			elseif isVal then
				Result.append_string ("val ")
			elseif isConcurrent then
				Result.append_string ("concurrent ")
			end -- if
		end -- if
		Result.append_string ("type " + name)
		if aliasName /= Void then
			Result.append_string (" alias " + aliasName)
		end

		n := formalGenerics.count
		if n > 0 then
			from
				Result.append_string (" [")
				i := 1
			until
				i > n
			loop
				Result.append_string (formalGenerics.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
			Result.append_character (']')
		end -- if

		n := parents.count
		if n > 0 then
			from
				Result.append_string (" extend ")
				i := 1
			until
				i > n
			loop
				Result.append_string (parents.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
		end -- if


		n := usage.count
		if n > 0 or else constants.count > 0 then
			from
				Result.append_string (" use ")
				i := 1
			until
				i > n
			loop
				Result.append_string (usage.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
			n := constants.count
			if n > 0 then
				from
					Result.append_string (" const ")
					i := 1
				until
					i > n
				loop
					Result.append_string (constants.item (i).out)
					if i < n then
						Result.append_string (", ")
					end
					i := i + 1
				end -- loop
			end -- if
		end -- if

		-- Result.append_character ('%N')
	
		n := memberSelections.count
		if n > 0 then
			from
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				incIdent
				Result.append_string (getIdent + "select ")
				i := 1
			until
				i > n
			loop
				Result.append_string (memberSelections.item (i).out)
				if i < n then
					Result.append_string (", ")
				end -- if
				i := i + 1
			end -- loop
			decIdent
			Result.append_character ('%N')
		end -- if

		n := inheritedOverrides.count
		if n > 0 then
			from
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				incIdent
				Result.append_string (getIdent + "override ")
				i := 1
			until
				i > n
			loop
				Result.append_string (inheritedOverrides.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
			decIdent
			Result.append_character ('%N')
		end -- if

		n := inhertitedInits.count
		if n > 0 then
			from
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				incIdent
				Result.append_string (getIdent + "new ")				
				i := 1
			until
				i > n
			loop
				Result.append_string (inhertitedInits.item (i).out)
				if i < n then
					Result.append_string (", ")
				end
				i := i + 1
			end -- loop
			decIdent
			Result.append_character ('%N')
		end -- if

		n := constObjects.count
		if n > 0 then
			from
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				incIdent
				Result.append_string (getIdent + "const:%N")
				incIdent
				Result.append_string (getIdent)
				i := 1
			until
				i > n
			loop
				Result.append_string (constObjects.item (i).out)
				if i < n then
					Result.append_string (", ")
				end -- if
				i := i + 1
			end -- loop
			decIdent
			decIdent
			Result.append_character('%N')
			Result.append_string(getIdent + "end%N")
		end -- if

		n := initMembers.count
		if n > 0 then
			from
				i := 1
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				Result.append_string (getIdent + "// " + n.out + " type initializer(s)%N")
				incIdent
			until
				i > n
			loop
				Result.append_string (getIdent + initMembers.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if

		n := unitMembers.count
		if n > 0 then
			from
				i := 1
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				Result.append_string (getIdent + "// " + n.out + " type member(s)%N")
				incIdent
			until
				i > n
			loop
				Result.append_string (getIdent + unitMembers.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if

		n := invariantPredicates.count
		if n > 0 then
			from
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if				
				Result.append_string (getIdent + "require%N")
				incIdent
				i := 1
			until
				i > n
			loop
				Result.append_string (getIdent + invariantPredicates.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if
		if Result.item (Result.count) /= '%N' then
			Result.append_character ('%N')
		end -- if
		Result.append_string (getIdent + "end // type " + name + "%N")
	end -- out
	setAliasName (aName: like aliasName) is
	do
		aliasName := aName
	end -- setAliasName
	infix "<" (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			if isExtension then
				Result := not other.isExtension
			elseif not other.isExtension then
				n := formalGenerics.count
				m := other.formalGenerics.count
				Result := n < m
				if not Result and then n = m then
					from
						i := 1
					until
						i > n
					loop
						if formalGenerics.item (i) < other.formalGenerics.item (i) then
							Result := True
							i := n + 1
						elseif formalGenerics.item (i).is_equal (other.formalGenerics.item (i)) then
							i := i + 1
						else
							i := n + 1
						end -- if
					end -- loop
				end -- if
			end -- if
		end -- if
	end
	is_equal (other: like Current): Boolean is
	local
		i, n: Integer
	do
		-- One source must have only one type extension for the particular type as < can not be defined for extension to the same type.
		Result := name.is_equal (other.name) and then isExtension = other.isExtension
		if Result then
			n := formalGenerics.count
			Result := n = other.formalGenerics.count
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if formalGenerics.item (i).is_equal (other.formalGenerics.item (i)) then
						i := i + 1
					else
						i := n + 1
						Result := False
					end -- if
				end -- loop
			end -- if
		end -- if		
	end	-- is_equal
	attach_pools (ast:CompilationUnitCompound) is
	do
		stringPool := ast.stringPool
		typePool := ast.typePool
	end -- attach_pools
	attach_use_pool (uc: like useConst) is
	require
		use_const_units_not_void: uc /= Void
	local
		i, n: Integer
	do
		useConst := uc
		from
			n := useConst.count
			i := 1
		until
			i > n
		loop
			typePool.add (useConst.item (i))
			i := i + 1
		end -- loop
	end -- attach_use_pool
	generate (cg: CodeGenerator) is
	local
		i, n: Integer
	do
		from
			i := 1
			n := initMembers.count
		until
			i > n
		loop
			initMembers.item (i).generate (cg)
			i := i + 1
		end -- loop

		from
			i := 1
			n := unitMembers.count
		until
			i > n
		loop
			unitMembers.item (i).generate (cg)
			i := i + 1
		end -- loop
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		i, n: Integer
	do
		from
			i := 1
			n := initMembers.count
		until
			i > n
		loop
			if initMembers.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop

		from
			i := 1
			n := unitMembers.count
		until
			i > n
		loop
			if unitMembers.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
	end -- checkValidity
	
feature {CompilationUnitCompound, SLangCompiler}
	useConst: Sorted_Array [UnitTypeNameDescriptor]
	stringPool: Sorted_Array [String]
	typePool: Sorted_Array[TypeDescriptor]

feature {None}
	
	init (aName: like name; is_final, is_ref, is_val, is_concurrent, is_virtual, is_extend: Boolean) is
	do
		name := aName
		isFinal		:= is_final
		isRef		:= is_ref
		isVal		:= is_val
		isConcurrent:= is_concurrent
		isVirtual	:= is_virtual
		isExtension	:= is_extend
		create formalGenerics.make (1, 0)
		create parents.make
		create usage.make
		create constants.make
		create memberSelections.make
		create inheritedOverrides.make	
		create inhertitedInits.make	
		create constObjects.make
		create unitMembers.make	
		create initMembers.make	
		create invariantPredicates.make (1, 0)
	end -- init
invariant	
	--non_void_unit_name: name /= Void
	consistent_extension: isExtension implies (not isFinal and then not isRef and then not isVal and then not isConcurrent and then not isVirtual)
	consistent_final: isFinal implies (not isVirtual)
	consistent_ref: isRef implies not isVal and then not isConcurrent
	consistent_val: isVal implies not isRef and then not isConcurrent
	consistent_con: isConcurrent implies not isRef and then not isVal	
	non_void_formalGenerics: formalGenerics /= Void
	non_void_parents: parents /= Void
	non_void_usage: usage /= Void
	non_void_importConstantsFrom: constants /= Void
    non_void_memberSelections: memberSelections /= Void
    non_void_inheritedOverrides: inheritedOverrides /= Void
    non_void_inhertitedInits: inhertitedInits /= Void
    non_void_constObjects: constObjects /= Void
    non_void_unitMembers: unitMembers /= Void
    non_void_initMembers: initMembers /= Void
    non_void_invariantPredicates: invariantPredicates /= Void
end 

class ParentDescriptor
-- ["~"] UnitTypeNameDescriptor 
inherit 
	Comparable
		redefine
			out, is_equal
	end
create
	init
feature {Any}
	isNonConformant: Boolean
	parent: UnitTypeNameDescriptor
	init (inc: Boolean; p: like parent) is
	require	
		non_void_parent: p /= Void
	do
		isNonConformant:= inc
		parent:= p
	end -- init
	is_equal (other: like Current): Boolean is
	do
		Result := parent.is_equal (other.parent)
	end
	infix "<"(other: like Current): Boolean is
	do
		Result := parent < other.parent
	end
	out: String is
	do
		if isNonConformant then
			Result := "~" + parent.out
		else
			Result := parent.out
		end
	end
invariant
	non_void_parent: parent /= Void
end -- class ParentDescriptor 

deferred class FormalGenericDescriptor
-- Identifier (["extend" Type ] ["new" [Signature]])| [":" (UnitTypeDescriptor | RoutineType)]
inherit
	SmartComparable
		undefine
			out
	end
	NamedTypeDescriptor
		rename
			getExternalName as uselessGetExternalName
		export
			{None} uselessGetExternalName
	end 
feature {Any}
	getExternalName (pos: Integer): String is
	require
		valid_formal_generic_position: pos > 0
	deferred
	ensure
		external_name_not_void: Result /= Void
	end -- getExternalName
	uselessGetExternalName: String is
	do
	end -- uselessGetExternalName
invariant
	name_not_void: name /= Void
end -- class FormalGenericDescriptor

class FormalGenericTypeNameDescriptor
-- Identifier
inherit
	FormalGenericDescriptor
	end
create
	init
feature {Any}
	out: String is
	do
		Result := "" + name
	end -- out
	getExternalName (pos: Integer): String is
	do
		Result := pos.out
		Result.append_string (buildHash (Result))
		-- Ignore constraint and init		
	end -- getExternalName
	
	init (n: like name) is
	require
		formal_generic_name_not_void: n /= Void
	do
		name := n
	end -- init

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
	end -- isNotLoaded
	
feature {FormalGenericDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
	end -- lessThan
end -- class FormalGenericTypeNameDescriptor


class FormalGenericTypeDescriptor
-- Identifier ["extend" Type ] ["new" [Signature]]
inherit
	FormalGenericDescriptor
	end
create
	init
feature {Any}
	typeConstraint: TypeDescriptor
	initConstraint: SignatureDescriptor
	out: String is
	do
		Result := "" + name
		if typeConstraint /= Void then
			Result.append_string(" extend ")
			Result.append_string (typeConstraint.out)
		end -- if
		if initConstraint /= Void then
			Result.append_string(" new ")
			Result.append_string (initConstraint.out)
		end -- if
	end -- out
	getExternalName (pos: Integer): String is
	do
		Result := pos.out
		Result.append_string (buildHash (Result))
		-- Ignore constraint and init
		
		--Result := "" + name
		--if typeConstraint /= Void then
		--	Result.append_string("_extend_")
		--	Result.append_string (typeConstraint.getExternalName)
		--end -- if
		--if initConstraint /= Void then
		--	Result.append_string("_new_")
		--	Result.append_string (initConstraint.getExternalName)
		--end -- if
		--Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	init (n: like name; tc: like typeConstraint; ic: like initConstraint) is
	require
		formal_generic_name_not_void: n /= Void
	local
	do
		name := n
		typeConstraint:= tc
		initConstraint:= ic
	end -- init

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if typeConstraint /= Void and then typeConstraint.isInvalid (context, o) then
			Result := True
		end -- if
	end -- is_invalid
	
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		if typeConstraint /= Void and then typeConstraint.isNotLoaded (context, o) then
			Result := True
		end -- if
-- Do we need to load all types from the signature?
--		if initConstraint /= Void and then initConstraint.isNotLoaded (context, o) then
--			Result := True
--		end -- if
	end -- isNotLoaded
	
feature {FormalGenericDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
	end -- lessThan
end -- class FormalGenericTypeDescriptor

class FormalGenericConstantDescriptor
-- Identifier ":" TypeDescriptor
inherit
	FormalGenericDescriptor
	end
create
	init
feature {Any}
	type: TypeDescriptor
	out: String is
	do
		Result := name + ": " + type.out
	end -- out
	getExternalName (pos: Integer): String is
	do
		Result := type.getExternalName
		--Result := name + "_" + type.getExternalName
		--Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	init (aName: String; ut: like type) is
	require
		name_not_void: aName /= Void
		type_not_void: ut /= Void
	do
		name := aName
		type := ut
	end -- init
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if type.isInvalid (context, o) then
			Result := True
		end -- if		
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		if type.isNotLoaded (context, o) then
			Result := True
		end -- if		
	end -- isNotLoaded


feature {FormalGenericDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := type.is_equal (other.type)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := type < other.type
	end -- lessThan
invariant
	type_not_void: type /= Void
end

class FormalGenericRoutineDescriptor
-- Identifier ":" RoutineType
inherit
	FormalGenericDescriptor
	end
create
	init
feature {Any}
	routineType: RoutineTypeDescriptor
	out: String is
	do
		Result := name + ": " + routineType.out
	end -- out
	getExternalName (pos: Integer): String is
	do
		Result := routineType.getExternalName
		--Result := name + "_" + routineType.getExternalName
		--Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	init (aName: String; rt: RoutineTypeDescriptor) is
	require
		name_not_void: aName /= Void
		type_not_void: rt /= Void
	do
		name := aName
		routineType := rt
	end -- init

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
	end -- isNotLoaded


feature {FormalGenericDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := routineType.is_equal (other.routineType)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := routineType < other.routineType
	end -- lessThan
invariant
	type_not_void: routineType /= Void
end -- class FormalGenericRoutineDescriptor

class SelectionDescriptor
--14 Identifier[Signature]
inherit
	Comparable
		redefine
			out, is_equal
	end
create	
	init
feature {Any}
	memberName: String
	memberSignature: SignatureDescriptor
	init (mn: String; ms: like memberSignature) is
	require
		name_not_void: mn /= Void
	do
		memberName := mn
		memberSignature := ms
	end -- init

	out: String is
	do
		Result := memberName
		if memberSignature /= Void then
			Result.append_string (memberSignature.out)
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	do
		Result := memberName.is_equal (other.memberName)
		if memberSignature /= Void and then other.memberSignature /= Void and then Result then
			Result := memberSignature.is_equal (other.memberSignature)
		end -- if
	end
	infix "<"(other: like Current): Boolean is
	do
		Result := memberName < other.memberName
		if memberSignature /= Void and then other.memberSignature /= Void and then Result then
			Result := memberSignature < other.memberSignature
		end -- if
	end
invariant
	name_not_void: memberName /= Void
end

class InitFromParentDescriptor
--15 UnitTypeName [Signature]
inherit	
	Comparable
		redefine
			out, is_equal
	end
create
	init
feature {Any}
	parent: UnitTypeNameDescriptor
	initSignature: SignatureDescriptor
	init (p: like parent; s: like initSignature) is 
	require
		non_void_parent: p /= Void
	do
		parent := p
		initSignature := s
	end -- init
	out: String is
	do
		Result := parent.out
		if initSignature /= Void then
			Result.append_string (initSignature.out)
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	do
		Result := parent.is_equal (other.parent)
		if Result then
			if initSignature = Void then
				Result := other.initSignature = Void
			elseif other.initSignature /= Void then
				Result := initSignature.is_equal (other.initSignature)
			else
				Result := False
			end -- if
		end -- if
	end
	infix "<"(other: like Current): Boolean is
	do
		Result := parent < other.parent
		if not Result and then parent.is_equal (other.parent) then
			if initSignature = Void then
				Result := other.initSignature /= Void
			elseif other.initSignature /= Void then
				Result := initSignature < other.initSignature
			else
				Result := False
			end -- if
		end -- if
	end
invariant
	non_void_parent: parent /= Void
end -- class InitFromParentDescriptor

class InheritedMemberOverridingDescriptor
-- UnitTypeNameDescriptor"."Identifier[SignatureDescriptor]
inherit	
	Comparable
		redefine
			out, is_equal
	end
create
	init
feature {Any}
	parent: UnitTypeNameDescriptor
	memberName: String
	signature: SignatureDescriptor
	init (p: like parent; mn: like memberName; s: like signature) is 
	require
		non_void_parent : p /= Void
		non_void_memberName : mn /= Void
	do
		parent := p
		memberName := mn
		signature := s
	end -- init
	out: String is
	do
		Result:= parent.out
		Result.append_string (".")
		Result.append_string (memberName)
		if signature /= Void then
			Result.append_string (signature.out)
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	do
		Result := parent.is_equal (other.parent) and then memberName.is_equal (other.memberName)
		if signature /= Void and then other.signature /= Void and then Result then
			Result := signature.is_equal (other.signature)
		end -- if
	end
	infix "<"(other: like Current): Boolean is
	do
		Result := parent < other.parent and then memberName < other.memberName
		if signature /= Void and then other.signature /= Void and then Result then
			Result := signature < other.signature
		end -- if
	end
invariant
	non_void_parent : parent /= Void
	non_void_memberName : memberName /= Void
end

deferred class MemberVisibilityDescriptor
-- "{"" [this| UnitTypeNameDescriptor {"," UnitTypeNameDescriptor}  ] "}"
inherit
	Any
		undefine out
	end
feature {Any}
	canAccess (client: UnitTypeNameDescriptor): Boolean is
	require
		client_not_void: client /= Void
	deferred
	end -- canAccess
end 
class SelectedVisibilityDescriptor
inherit
	MemberVisibilityDescriptor
	end
create
	init
feature {Any}
	clients: Sorted_Array[UnitTypeNameDescriptor]
	canAccess (client: UnitTypeNameDescriptor): Boolean is
	do
		Result := clients.search (client) /= Void
	end -- canAccess
	init is -- (c: like clients) is
	do
		create clients.make
		--if c = Void then
		--	create clients.make
		--else
		--	clients := c
		--end -- if
	end -- init
	out: String is
	local	
		i, n: Integer
	do
		Result := "{"
		from
			i := 1
			n := clients.count
		until
			i > n
		loop
			Result.append_string (clients.item (i).out)
			if i < n then
				Result.append_string (", ")
			end
			i := i + 1
		end -- loop
		Result.append_character ('}')
		Result.append_character (' ')
	end
invariant
	non_void_clients: clients /= Void
end
class AnyVisibilityDescriptor
inherit
	MemberVisibilityDescriptor
	end
feature {Any}
	out: String is do Result := "{Any} " end  
	canAccess (client: UnitTypeNameDescriptor): Boolean is
	once
		Result := True
	end -- canAccess
end -- class AnyVisibilityDescriptor
class NoneVisibilityDescriptor
inherit
	MemberVisibilityDescriptor
	end
feature {Any}
	out: String is 
	do
		Result := "{} "
	end -- out
	canAccess (client: UnitTypeNameDescriptor): Boolean is
	once
		-- Result := False
	end -- canAccess
end -- class NoneVisibilityDescriptor
class PrivateVisibilityDescriptor
inherit
	MemberVisibilityDescriptor
	end
feature {Any}
	out: String is
	do
		Result := "{this} "
	end -- out
	canAccess (client: UnitTypeNameDescriptor): Boolean is
	once
		-- Result := False
	end -- canAccess
end -- class PrivateVisibilityDescriptor

deferred class MemberDeclarationDescriptor
-- [MemberVisibility] ([override] [final] UnitAttribiteDeclaration|UnitRoutineDeclaration) | InitDeclaration
inherit
	SmartComparable
		redefine
			out, is_equal, infix "<"
	end
	SourcePosition
		redefine
			out, is_equal
	end 
	BuildServer
		redefine
			out, is_equal
	end
feature {Any}
	visibility: MemberVisibilityDescriptor
	isOverriding: Boolean is
	deferred
	end -- isOverriding
	isFinal: Boolean is
	deferred
	end -- isFinal
	out: String is
	do
		if visibility = Void then
			Result := "{Any} "
		else
			Result := visibility.out
		end -- if
		if isOverriding then
			Result.append_string ("override ")
		end -- if
		if isFinal then
			Result.append_string ("final ")
		end -- if
	end -- out
	
	cutImplementation is
	deferred
		-- redefine in a proper descendant
	end  -- cutImplementation
	
	setVisibility (v: like visibility) is
	require
		non_void_visibility: v /= Void
	do
		visibility := v
	end -- setVisibility
	
	name: String is
	deferred
	ensure
		name_not_void: Result /= Void
	end -- name
	is_equal (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
		if Result then
			if same_type (other) then
				Result := sameAs (other)
			else
				Result := false
			end -- if
		end -- if
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		Result := name < (other.name)
		if not Result and then name.is_equal (other.name) and then same_type (other) then
			Result := lessThan (other)
		end -- if
	end -- infix "<"
end -- class MemberDeclarationDescriptor

deferred class RoutineDescriptor
inherit
	Comparable
		undefine
			out, is_equal
	end
	SourcePosition
		undefine 	
			out
	end
	BuildServer
		undefine
			out, is_equal
	end
feature {Any}
	isPure: Boolean
	isSafe: Boolean	
	parameters: Array [ParameterDescriptor]
	type: TypeDescriptor
	usage: Sorted_Array [EnclosedUseEementDescriptor]
	constants: Sorted_Array [UnitTypeNameDescriptor] -- FullUnitNameDescriptor]
	preconditions: Array [PredicateDescriptor]
	isForeign: Boolean
	isOneLine: Boolean is do
		Result := expr /= Void -- and then innerBlock = Void
	end -- isOneLine
	expr: ExpressionDescriptor
	innerBlock: InnerBlockDescriptor
	postconditions: Array [PredicateDescriptor]	
	
	name: String is
	deferred
	end -- name
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		i, n: Integer
	do
debug
	o.putLine ("Validity check for routine `" + name + "`")
end -- debug
		if parameters /= Void then
			from
				i := 1
				n := parameters.count
			until
				i > n
			loop
				if parameters.item(i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
		end -- if
		if type /= Void and then type.isInvalid (context, o) then
			Result := True
		end -- if
		if preconditions /= Void then
			from
				i := 1
				n := preconditions.count
			until
				i > n
			loop
				if preconditions.item(i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
		end -- if
		if innerblock = Void then
			if expr = Void then 
				-- none body !!!
			elseif expr.isInvalid (context, o) then
				Result := True
			else -- check if type of 'expr' conforms to the type of function
			end -- if
		elseif innerblock.isInvalid (context, o) then
			Result := True
		end -- if
		if postconditions /= Void then
			from
				i := 1
				n := postconditions.count
			until
				i > n
			loop
				if postconditions.item(i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- is_invalid	
	
	
end -- class RoutineDescriptor

deferred class UnitRoutineDescriptor
inherit
	MemberDeclarationDescriptor
		rename
			out as memberOut
		export {None} memberOut
	end
	RoutineDescriptor
		undefine
			is_equal
		select	
			out
	end
	Identation
		undefine
			is_equal
	end
feature {Any}

	isVirtual: Boolean is
	deferred
	end -- isVirtual

	sameAs (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := type = other.type
		if not Result and then type /= Void and then other.type /= Void then
			Result := type.is_equal (other.type)
		end -- if
		if Result then
			if parameters /= Void then
				n := parameters.count
			end -- if
			if other.parameters /= Void then
				m := other.parameters.count
			end -- if
			Result := n = m
			if Result and then parameters /= Void and then other.parameters /= Void then
				from
					i := 1
				until
					i > n
				loop
--					if parameters.item (i).sameAs (other.parameters.item (i)) then
					if parameters.item (i).is_equal (other.parameters.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- if
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		if parameters /= Void then
			n := parameters.count
		end -- if
		if other.parameters /= Void then
			m := other.parameters.count
		end -- if
		Result := n < m
		if not Result and then n = m and then parameters /= Void and then other.parameters /= Void then
			from
				i := 1
			until
				i > n
			loop
--				if parameters.item (i).lessThan (other.parameters.item (i)) then
				if parameters.item (i) < other.parameters.item (i) then
					Result := True
					i := n + 1
--				elseif parameters.item (i).sameAs (other.parameters.item (i)) then
				elseif parameters.item (i).is_equal (other.parameters.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- if
		end -- if
	end -- lessThan

	aliasName: String is do end -- aliasName
	
	out: String is
	local
		i, n: Integer
	do
		if name.is_equal ("<>") then
			Result := "rtn"
		else
			Result := memberOut
			if isPure then
				if Result.count > 0 and then Result.item (Result.count) /= ' ' then
					Result.append_character (' ')
				end -- if
				Result.append_string ("pure ")
			elseif isSafe then
				if Result.count > 0 and then Result.item (Result.count) /= ' ' then
					Result.append_character (' ')
				end -- if
				Result.append_string ("safe ")
			end -- if
			Result.append_string (name)
		end -- if
		if aliasName /= Void then
			if Result.item (Result.count) /= ' ' then
				Result.append_character (' ')
			end -- if
			Result.append_string ("alias ")
			Result.append_string (aliasName)
		end -- if
		if parameters /= Void then
			n := parameters.count
			if n > 0 then
				from
					Result.append_string (" (")
					i := 1
				until
					i > n
				loop
					Result.append_string (parameters.item (i).out)
					if i < n then
						Result.append_string ("; ")
					end -- if
					i := i + 1
				end -- loop
				Result.append_character (')')
			end -- if
		end -- if
		if type /= Void then
			Result.append_character(':')
			Result.append_character(' ')
			Result.append_string (type.out)
		end -- if
		if usage /= Void then
			n := usage.count
			if n > 0 then
				from
					Result.append_string (" use ")
					i := 1
				until
					i > n
				loop
					Result.append_string (usage.item (i).out)
					if i < n then
						Result.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				outConstants (Result, " const ")
			else
				outConstants (Result, " use const ")
			end -- if
		else
			outConstants (Result, " use const ")
		end -- if
		if preconditions /= Void then
			n := preconditions.count
			if n > 0 then
				from
					if Result.item (Result.count) /= '%N' then
						Result.append_character ('%N')
					end -- if
					Result.append_string (getIdent + "require%N")
					incIdent
					i := 1
				until
					i > n
				loop
					Result.append_string (getIdent + preconditions.item (i).out)
					if Result.item (Result.count) /= '%N' then
						Result.append_character ('%N')
					end -- if
					i := i + 1
				end -- loop
				decIdent
			end -- if
		end -- if

		if isVirtual then
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			Result.append_string (getIdent + "abstract%N")
		elseif isForeign then
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			Result.append_string (getIdent + "foreign%N")
		elseif isOneLine then
			Result.append_string (" => ")
			--if expr = Void then
			--	Result.append_string ("none")
			--else
				Result.append_string (expr.out)
			--end -- if
			Result.append_character('%N')
		elseif innerBlock = Void then
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			Result.append_string (getIdent + "none")
		else
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			Result.append_string (innerBlock.out)
		end -- if

		if postconditions /= Void then
			n := postconditions.count
			if n > 0 then
				from
					if Result.item (Result.count) /= '%N' then
						Result.append_string ("%N")
					end -- if
					Result.append_string (getIdent + "ensure%N")
					incIdent
					i := 1
				until
					i > n
				loop
					Result.append_string (getIdent + postconditions.item (i).out)
					if Result.item (Result.count) /= '%N' then
						Result.append_string ("%N")
					end -- if
					i := i + 1
				end -- loop
				decIdent
			end -- if
		end -- if
		if not ((isVirtual or else isForeign or else isOneLine) and then (postconditions = Void or else postconditions.count = 0)) then
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			Result.append_string (getIdent + "end // " + name + "%N")	
		end -- if
	end -- out
	cutImplementation is
	do
		innerBlock := Void
		expr := Void
	end -- cutImplementation
feature {None}
	outConstants (aResult: String; aTitle: String) is
	require
		aResult /= Void
		aTitle /= Void
	local
		i, n: Integer
	do
		if constants /= Void then
			n := constants.count
			if n > 0 then
				from
					aResult.append_string (aTitle)
					i := 1
				until
					i > n
				loop
					aResult.append_string (constants.item (i).out)
					if i < n then
						aResult.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				aResult.append_character('%N')
			end -- if
		end -- if
	end -- outConstants
invariant
	is_virtual_consistent: isVirtual implies innerBlock = Void	and then expr = Void
	is_foreign_consistent: isForeign implies innerBlock = Void	and then expr = Void
	is_one_line_consistent: isOneLine implies innerBlock = Void --and then expr /= Void
end -- class UnitRoutineDescriptor

class UnitRoutineDeclarationDescriptor
-- [pure|safe] RoutineName [final Identifier] [UnitRoutineParameters] [":" Type] [EnclosedUseDirective]
-- [RequireBlock]
-- ((InnerBlock) [EnsureBlock] BlockEnd) | ((abstract|foreign|none| (‚Äú=>‚ÄùExpression))[EnsureBlock BlockEnd])
inherit
	UnitRoutineDescriptor
		redefine
			aliasName
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature {Any}
	--name: String
	aliasName: String
	isVirtual: Boolean
	isOverriding: Boolean
	isFinal: Boolean
	--type: TypeDescriptor
	--expr: ExpressionDescriptor
	init (isO, isFi, isP, isS: Boolean; aName: like name; anAliasName: like aliasName; p: like parameters; t: like type; u: like usage; c: like constants; pre: like preconditions; isF, isV: Boolean; b: like innerBlock; e: like expr; post: like postconditions) is
	require
		name_not_void: aName /= Void
		expr_consistency: e /= Void implies b = Void
		body_consistency: b /= Void implies e = Void and then not isV and then not isF
	do
		parameters := p
		type := t
		usage := u
		constants := c
		preconditions := pre
		isForeign := isF
		innerBlock := b
		postconditions := post
		name := aName
		aliasName := anAliasName
		isVirtual := isV
		isOverriding := isO
		isFinal := isFi
		isPure := isP
		isSafe := isS
		expr := e
	end -- init
	generate (cg: CodeGenerator) is
	do
	end -- generate
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	do
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- is_invalid
	
invariant
	--name_not_void: name /= Void
	expr_consistency: expr /= Void implies innerBlock = Void
	body_consistency: innerBlock /= Void implies expr = Void and then not isVirtual and then not isForeign
end -- class UnitRoutineDeclarationDescriptor

class InitDeclarationDescriptor
-- UnitName [UnitRoutineParameters] [EnclosedUseDirective] 
-- [RequireBlock] 
-- (InnerBlock [EnsureBlock] BlockEnd)|(foreign|none [EnsureBlock BlockEnd])
inherit
	UnitRoutineDescriptor
	end
create
	init
feature {Any}
	isVirtual: Boolean is False
	isOverriding: Boolean is False
	isFinal: Boolean is False
	name: String is
	do
		Result := unitDsc.name		
	end -- name
	unitDsc: UnitDeclarationDescriptor
	init (ud: like unitDsc; currentVisibilityZone: like visibility; p: like parameters; u: like usage; c: like constants; pre: like preconditions; isF: Boolean; b: like innerBlock; post: like postconditions) is
	require
		current_unit_not_void: ud /= Void
	do
		unitDsc := ud
		visibility := currentVisibilityZone
		parameters := p
		usage := u
		constants := c
		preconditions := pre
		isForeign := isF
		innerBlock := b
		postconditions := post
	end -- init
	generate (cg: CodeGenerator) is
	do
	end -- generate
-- class interface INITDECLARATIONDESCRIPTOR
-- inherit UNITROUTINEDESCRIPTOR
-- creation init
-- feature 
-- 
--    col: INTEGER
--    row: INTEGER
-- 
--    isVirtual: BOOLEAN
--    isOverriding: BOOLEAN
--    isFinal: BOOLEAN
--    isforeign: BOOLEAN
--    isoneline: BOOLEAN
--    ispure: BOOLEAN
--    issafe: BOOLEAN
-- 
--    unitDsc: UNITDECLARATIONDESCRIPTOR
--    aliasname: STRING
-- 
-- 
--    constants: SORTED_ARRAY [UNITTYPENAMEDESCRIPTOR]
--    usage: SORTED_ARRAY [ENCLOSEDUSEEEMENTDESCRIPTOR]
-- 
--    expr: EXPRESSIONDESCRIPTOR
-- 
--    visibility: MEMBERVISIBILITYDESCRIPTOR
--    parameters: ARRAY [PARAMETERDESCRIPTOR]
--    type: TYPEDESCRIPTOR
--    preconditions: ARRAY [PREDICATEDESCRIPTOR]
--    innerblock: INNERBLOCKDESCRIPTOR
--    postconditions: ARRAY [PREDICATEDESCRIPTOR]
-- 
-- end interface -- class INITDECLARATIONDESCRIPTOR

end -- class InitDeclarationDescriptor

     
-- AliasName : alias Identifier
-- RoutineName : ( Identifier [Identifier] )|ì()î|ì:=î|(OperatorName [AliasName]) OperatorName : OperatorSign [OperatorSign]
-- OperatorSign : ì=î | ì/î | î<î | î>î | ì+î | ì-ì | ì*î | ì\î | ì^î | î&î | î|î

--class ConstObjectsDeclarationDescriptor
-- const [ ConstObjectDescriptor { ì,î ConstObjectDescriptor} ] end
--feature {Any}
--	constObjects: Array [ConstObjectDescriptor]
--end

deferred class ConstObjectDescriptor
--20 ConstObject : ( Constant | (Idenitifer [ Arguments ]) [ ì..î  Constant | (Idenitifer [ì.îinit] [ Arguments ]) ] ) | (ì{î RegularExpression ì}î IntegerConstant [ì+î])
-- RegularExpression: Constant ({ì|îConstant}) | (ì|î î..î Constant)
inherit	
	SmartComparable
		undefine 
			out 
	end
end -- class ConstObjectDescriptor

class RegExpDescriptor
-- RegularExpression:
-- Constant {"|" Constant}
-- |
-- Constant "|" ".." Constant
inherit
	Comparable
		redefine
			out, is_equal
	end
create
	init
feature {Any}
	isPeriod: Boolean
	constants: Array [ConstantDescriptor]
	init (ip: Boolean; c: like constants) is
	require
		non_void_constants: c /= Void and then ( ip implies c.count = 2 or else not ip implies c.count > 0) 
	do
		isPeriod:= ip
		constants:= c
	end -- init
	out: String is
	local
		i, n : Integer
	do
		if isPeriod then
			Result := constants.item (1).out + " |.. " + constants.item (2).out
		else
			Result := ""
			Result.append_string (constants.item (1).out)
			from
				i := 2
				n := constants.count
			until
				i > n
			loop
				Result.append_character ('|')
				Result.append_string (constants.item (i).out)
				i := i + 1
			end -- loop
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	local
		i, n : Integer
	do
		Result := isPeriod = other.isPeriod
		if Result then
			n := constants.count
			Result := n = other.constants.count
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if constants.item (i).is_equal (other.constants.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	local
		i, n, m : Integer
	do
		Result := not isPeriod and then other.isPeriod
		if not Result and then isPeriod = other.isPeriod then
			n := constants.count
			m := other.constants.count
			Result := n < m
			if not Result and then n = m then
				from
					i := 1
				until
					i > n
				loop
					if constants.item (i) < other.constants.item (i) then
						Result := True
						i := n + 1
					elseif constants.item (i).is_equal (other.constants.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- infix "<"
invariant
	non_void_constants: constants /= Void and then ( isPeriod implies constants.count = 2 or else not isPeriod implies constants.count > 0) 
end -- class RegExpDescriptor

class RegExpConstObjectDescriptor
-- Regular expression:
-- "{" RegularExpression "}" IntegerConstant ["+"]
-- RegularExpression:
-- Constant {"|" Constant}
-- |
-- Constant "|" ".." Constant
inherit
	ConstObjectDescriptor
	end
create
	init
feature {Any}
	regExp: RegExpDescriptor
	intConst: ConstantDescriptor
	hasPlus: Boolean
	init (re: like regExp; ic: like intConst; hp: Boolean) is
	require
		non_void_reg_exp: regExp /= Void
		non_void_cont_dsc: intConst /= Void
	do
		regExp:= re
		intConst:= ic
		hasPlus:= hp
	end -- init
	out: String is
	do
		Result := "{" + regexp.out + "} " + intConst.out
		if hasPlus then
			Result.append_character ('+')
		end -- if
	end -- if
	sameAs (other: like Current): Boolean is
	do
		Result := hasPlus = other.hasPlus and then regExp.is_equal (other.regExp) and then intConst.is_equal (other.intConst)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := not hasPlus and then other.hasPlus
		if not Result and then hasPlus and then other.hasPlus then
			Result := regExp < other.regExp
			if not Result and then regExp.is_equal (other.regExp) then
				Result := intConst < other.intConst
			end -- if
		end -- if
	end -- lessThan
invariant
	non_void_reg_exp: regExp /= Void
	non_void_cont_dsc: intConst /= Void
end -- class RegExpConstObjectDescriptor

class ConstRangeObjectDescriptor
-- Constant | (Idenitifer [ Arguments ]) ".."  Constant | (Idenitifer [ Arguments ])
inherit
	ConstObjectDescriptor
	end
create
	init
feature {Any}
	coDsc1, coDsc2: ConstObjectDescriptor
	init (c1, c2: ConstObjectDescriptor) is
	require
		non_void_co1: c1 /= Void
		non_void_co2: c2 /= Void
	do
		coDsc1 := c1
		coDsc2 := c2
	end -- init
	out: String is
	do
		Result := coDsc1.out + " .. " + coDsc2.out
	end -- if
	sameAs (other: like Current): Boolean is
	do
		Result := coDsc1.is_equal (other.coDsc1) and then coDsc2.is_equal (other.coDsc2)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := coDsc1 < other.coDsc1
		if not Result and then coDsc1.is_equal (other.coDsc1) then
			Result := coDsc2 < other.coDsc2
		end -- if
	end -- lessThan
invariant
	non_void_co1: coDsc1 /= Void
	non_void_co2: coDsc2 /= Void
end -- class ConstRangeObjectDescriptor

class ConstWithIteratorDescriptor
inherit
	ConstObjectDescriptor
	end
create
	init
feature {Any}
	name: ConstObjectDescriptor -- String
	operator: String
	exprDsc: ExpressionDescriptor	
	init (n: like name; o: like operator; e: like exprDsc) is
	require
		name_not_void: n /= Void
		operator_not_void: o /= Void
		expression_not_void: e /= Void
	do
		name := n
		operator := o
		exprDsc := e
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name) and then exprDsc.is_equal (other.exprDsc)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			Result := operator < other.operator
			if not Result and then operator.is_equal (other.operator) then
				Result := exprDsc < other.exprDsc
			end -- if			
		end -- if
	end -- lessThan
	out: String is
	do
		Result := ""
		Result.append_string (name.out)
		Result.append_character ('{')
		Result.append_string (operator)
		Result.append_character (' ')
		Result.append_string (exprDsc.out)
		Result.append_character ('}')
	end -- out
invariant
	name_not_void: name /= Void
	operator_not_void: operator /= Void
	expression_not_void: exprDsc /= Void
end -- class ConstWithIteratorDescriptor

class ConstWithInitDescriptor
inherit
	ConstObjectDescriptor
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature {Any}
	--name: String --ConstObjectDescriptor
	arguments: Array [ExpressionDescriptor]
	init (n: like name; args: like arguments) is
	require
		name_not_void: n /= Void
	do
		name := n
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
	end -- init
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := name.is_equal (other.name)
		if Result then
			n := arguments.count 
			Result := n = other.arguments.count 
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if arguments.item (i).is_equal (other.arguments.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			n := arguments.count 
			m := other.arguments.count
			Result := n < m
			if not Result and then n = m then
				from
					i := 1
				until
					i > n
				loop
					if arguments.item (i) < other.arguments.item (i) then
						Result := True
						i := n + 1
					elseif arguments.item (i).is_equal(other.arguments.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- lessThan
	out: String is
	local
		i, n: Integer
	do
		Result := ""
		Result.append_string (name.out)
		n := arguments.count 
		if n > 0 then
			from
				Result.append_string (" (")
				i := 1
			until
				i > n
			loop
				Result.append_string (arguments.item (i).out)
				if i < n then
					Result.append_string (", ")
				end -- if
				i := i + 1
			end -- loop
			Result.append_character (')')
		end -- if
	end -- out
invariant
	--name_not_void: name /= Void
	arguments_not_void: arguments /= Void
end -- class ConstWithInitDescriptor

-- RegularExpression:
--      Constant ({"|" Constant}) | ("|" ".." Constant)

deferred class BuildServer
feature
	buildHash (aString: String): String is
	require
		non_void_string: aString /= Void
	do
		Result := aString.hash_code.out
		Result := ""
	end -- buildHash
	generate (cg: CodeGenerator) is
	deferred
	end -- generate
	isInvalid (context: CompilationUnitCommon; o: Output): Boolean is
	require
		non_void_contex: context /= Void
		non_void_output: o /= Void
	do
		if not validityChecked then
			Result := is_invalid (context, o)
			isValid := not Result
			validityChecked := True
		end -- if
	end -- isInvalid
	validityChecked: Boolean
	isValid: Boolean
feature {None}
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	require
		non_void_contex: context /= Void
		non_void_output: o /= Void
	deferred
	end -- is_invalid
end -- class BuildServer

deferred class StatementDescriptor
-- AssignmentStatementDescriptor | LocalAttributeCreation | MemberCallOrCreation | IfCase | LoopStatementDescriptor | BreakStatementDescriptor | DetachStatementDescriptor
--    | ReturnStatementDescriptor | HyperBlockDescriptor | RaiseStatementDescriptor 
inherit
	SmartComparable
		undefine
			out
	end
	SourcePosition
		undefine
			out, is_equal
	end
	BuildServer
		undefine
			out, is_equal
	end
feature {StatementDescriptor}
	sameAs (other: like Current): Boolean is
	once
print ("StatementDescriptor.sameAs:  never should be called !!!%N")
	end -- sameAs
	lessThan (other: like Current): Boolean is
	once
print ("StatementDescriptor.lessThan: never should be called !!!%N")
	end -- lessThan
end -- class StatementDescriptor

class RepeatWhileDescriptor
inherit	
	LoopStatementDescriptor
		rename
			init as noInit
--		export
--			{Any} whileExpr,
--			{None} all
		redefine
			out
	end
create
	init
feature {Any}
	--expression: ExpressionDescriptor
	init (e: like whileExpr) is
	require
		non_void_whileExpr: e /= Void
	do
		--expression := e
		whileExpr:= e
		create requireClause.make (1, 0)
		create ensureClause.make (1, 0)
		create invariantOffList.make
		create statements.make (1, 0)
		create whenClauses.make (1, 0)
		create whenElseClause.make (1, 0)		
	end -- init
	out: String is 
	do
		Result := "while " + whileExpr.out
	end -- out
--invariant
--	non_void_whileExpr: expression /= Void
end -- class RepeatWhileDescriptor

class DetachStatementDescriptor
-- ? Identifier
inherit	
	StatementDescriptor
	end
create
	init
feature {Any}
	id: String

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	out: String is
	do
		Result := "?" + id
	end -- out
	init (name: like id) is
	require
		non_void_identifier: name /= Void
	do
		id := name
	end
invariant
	non_void_identifier: id /= Void
end -- class DetachStatementDescriptor

class RaiseStatementDescriptor
-- raise (Expression | ";")
inherit	
	StatementDescriptor
	end
create	
	init
feature {Any}
	expr: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	out: String is
	do
		Result := "raise"
		if expr = Void then
			Result.append_character (';')
		else
			Result.append_character (' ')
			Result.append_string (expr.out)
		end -- if
	end -- out
	init (e: like expr) is
	do
		expr := e
	end -- init
end -- class RaiseStatementDescriptor

class ReturnStatementDescriptor
-- return (Expression | ";")
inherit	
	StatementDescriptor
	end
create
	init
feature {Any}
	expr: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	do
		Result := "return"
		if expr = Void then
			Result.append_character (';')
		else
			Result.append_character (' ')
			Result.append_string (expr.out)
		end -- if
	end -- out
	init (e: like expr) is
	do
		expr := e
	end
end -- class ReturnStatementDescriptor

class HyperBlockDescriptor
-- [RequireBlock] InnerBlockDescriptor [EnsureBlock] end
inherit
	InnerBlockDescriptor
		rename
			init as InnerBlockInit
		export
			{None} InnerBlockInit
		redefine
			out
	end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}
	requireClause: Array [PredicateDescriptor]
	ensureClause: Array [PredicateDescriptor]
	out: String is
	local
		i, n: Integer
	do
		Result := ""
		n := requireClause.count
		if n > 0 then
			from
				Result.append_string (getIdent + "require%N")
				incIdent
				i := 1
			until
				i > n
			loop
				Result.append_string (getIdent + requireClause.item(i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i +1 
			end
			decIdent
		--else
		--	Result.append_character('%N')
		end -- if
		Result.append_string (Precursor)
		n := ensureClause.count
		if n > 0 then
			from
				Result.append_string (getIdent + "ensure%N")
				incIdent
				i := 1
			until
				i > n
			loop
				Result.append_string (getIdent + ensureClause.item(i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end --if
				i := i +1 
			end -- loop
			decIdent
		end -- if
		Result.append_string (getIdent + "end // block%N")
	end -- out
	init (rc: like requireClause; invOff: like invariantOffList; stmts: like statements; wc: like whenClauses; wec: like whenElseClause; ec: like ensureClause) is
	do
		InnerBlockInit (invOff, stmts, wc, wec)
		if rc = Void then
			create requireClause.make (1, 0)
		else
			requireClause := rc
		end -- if
		if ec = Void then
			create ensureClause.make (1, 0)
		else
			ensureClause := ec
		end -- if
	end
invariant
	non_void_requireClause: requireClause /= Void
	non_void_ensureClause: ensureClause /= Void
end -- class HyperBlockDescriptor

class EntityDeclarationStatementDescriptor
-- [const|rigid] indentifier "is" Expression
inherit	
	StatementDescriptor
	end
create	
	init
feature {Any}
	isConst,
	isRigid,
	isVar: Boolean
	entity: String
	expr: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
		-- check entity !!!
		
		if expr.isInvalid (context, o) then 
			Result := True
		end -- if
		if not Result then
			-- not_implemened_yet
			-- entity is to be visible here
			-- type of expr should be compatible to the type of entity
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	do
		if isConst then
			Result := "const "
		elseif isRigid then
			Result := "rigid "
		elseif isVar then
			Result := "var "
		else
			Result := ""
		end  -- if
		Result.append_string (entity + " is " + expr.out)
	end
	init (iv, ic, ir: Boolean; anEntity: like entity; anExpr: like expr) is
	require
		non_void_entity: anEntity /= Void
		non_void_expresssion: anExpr /= Void
	do
		isConst := ic
		isRigid := ir
		isVar	:= iv
		entity 	:= anEntity
		expr	:= anExpr
	end -- init
invariant
	non_void_entity: entity /= Void
	non_void_expresssion: expr /= Void
	consistent_const: isConst implies not isRigid and then not isVar
	consistent_rigid: isRigid implies not isConst and then not isVar
	consistent_var: isVar implies not isRigid and then not isConst
end -- class EntityDeclarationStatementDescriptor

class AssignmentStatementDescriptor
-- Writable ":=" Expression
inherit	
	StatementDescriptor
	end
create	
	init
feature {Any}
--	writable: CallDescriptor
	writable: ExpressionDescriptor
	expr: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		--notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		if writable.isInvalid (context, o) then 
			Result := True
		end -- if
		if expr.isInvalid (context, o) then 
			Result := True
		end -- if
		if not Result then
			-- not_implemened_yet
			-- writable is to be visible here
			-- type of expr should be compatible to the type of writable
-- not_implemented_yet: AssignmentStatementDescriptor.isInvalid
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	do
		Result := writable.out + " := " + expr.out
		-- Result := writable.out + ".:= (" + expr.out + ")"
	end
	init (w: like writable; e: like expr) is
	require
		non_void_writable: w /= Void
		non_void_expresssion: e /= Void
	do
		writable:= w
		expr	:= e
	end
invariant
	non_void_writable: writable /= Void
	non_void_expresssion: expr /= Void
end -- class AssignmentStatementDescriptor

deferred class NamedDescriptor
inherit	
	Any
		undefine	
			out
	end
feature
	name: String
invariant
	non_void_name: name /= Void
end -- class NamedDescriptor
---------------- Entities start -----------------
-- UnitAttributeNamesList:
--  [const | rigid] Identifier {","[const | rigid] Identifier}
-- LocalAttributeNamesList:
--  [var | rigid] Identifier {","[var | rigid] Identifier}

deferred class EntityDeclarationDescriptor
inherit
	Comparable
		redefine
			out, is_equal
	end
	SourcePosition
		redefine
			out, is_equal
	end
	NamedDescriptor
		redefine
			is_equal
	end
feature {Any}		
	markedVar: Boolean is
	deferred
	end -- isVar
	markedRigid: Boolean is
	deferred
	end -- isRigid
	markedConst: Boolean is
	deferred
	end -- isConst
	--name: String
	type: TypeDescriptor
	expr: ExpressionDescriptor
	--type: TypeDescriptor is
	--deferred
	--end -- type
	out: String is
	do
		if markedVar then
			Result := "var "
		elseif markedRigid then
			Result := "rigid "
		elseif markedConst then
			Result := "const "
		else
			Result := ""
		end -- if
		Result.append_string (name)
		if type /= Void then
			Result.append_character (':')
			Result.append_character (' ')
			Result.append_string (type.out)
		end -- if
		if expr /= Void then
			Result.append_string (" is ")
			Result.append_string (expr.out)
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- is_equal
	infix "<"(other: like Current): Boolean is
	do
		Result := name < other.name
	end -- infix "<"
	--setName (tmpDsc: TemporaryLocalAttributeDescriptor) is
	setName (tmpDsc: LocalAttrDeclarationDescriptor) is
	require
		non_void_tmpDsc: tmpDsc /= Void
	do
		name := tmpDsc.name
		setFlags (tmpdsc)
	end -- setName	
	--setFlags (tmpDsc: TemporaryLocalAttributeDescriptor) is
	setFlags (tmpDsc: LocalAttrDeclarationDescriptor) is
	require
		non_void_tmpDsc: tmpDsc /= Void
	do
	end -- setFlags
invariant
	--non_void_name: name /= Void
	consistent_marked_var	: markedVar	  implies (not markedRigid and then not markedConst)
	consistent_marked_rigid	: markedRigid implies (not markedVar   and then not markedConst)
	consistent_marked_const	: markedConst implies (not markedVar   and then not markedRigid)
end -- class EntityDeclarationDescriptor

--deferred 
class UnitAttributeDeclarationDescriptor
inherit
	EntityDeclarationDescriptor
		undefine
			is_equal, infix "<"
		redefine
			out
		select
			out
	end
	MemberDeclarationDescriptor
		rename
			out as memberOut
			export {NONE} memberOut
--		undefine
--			is_equal, infix "<"
	end
create
	init, init_for_search
feature {Any}
	isFinal: Boolean
	isOverriding: Boolean
	assigner: AttributeAssignerDescriptor
	markedVar: Boolean is False
	markedConst: Boolean
	markedRigid: Boolean
	--type: TypeDescriptor
	--expr: ExpressionDescriptor

	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := name < other.name
	end -- lessThan
	
	generate (cg: CodeGenerator) is
	do
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- is_invalid		
	
	cutImplementation is
	do
		expr := Void
		if assigner /= Void then
			assigner.cutImplementation
		end -- if
	end -- cutImplementation
	
	init_for_search (isC, isR: Boolean; aName: like name) is
	require
		name_not_void: aName /= Void
	do
		markedConst := isC
		markedRigid := isR
		name := aName
	end -- init_for_search
	init (isO, isF, mc, mr: Boolean; aName: String; aType: like type; a: like assigner; ie: like expr) is
	require
		non_void_name: aName /= Void
		consistent_expr_and_type : ie = Void implies aType /= Void
		consistent_flags: mc implies not mr and then mr implies not mc
	do
		isOverriding := isO
		isFinal := isF
		markedConst := mc
		markedRigid	:= mr
		name	:= aName
		type	:= aType
		assigner:= a
		expr:= ie
	end -- init

	out: String is
	do
		Result := memberOut
		Result.append_string (Precursor)
		-- Result.append_string (name)
		--if type /= Void then
		--	Result.append_character (':')
		--	Result.append_character (' ')
		--	Result.append_string (type.out)
		--end -- if
		--if expr /= Void then
		--	Result.append_string (" is ")
		--	Result.append_string (expr.out)
		--end -- if
		if assigner /= Void then
			Result.append_character (' ')
			Result.append_string (assigner.out)
		end -- if
	end -- out

end -- class UnitAttributeDeclarationDescriptor

--deferred class LocalAttrDeclarationDescriptor
--inherit
--	EntityDeclarationDescriptor
--	end
--	StatementDescriptor
--		undefine
--			is_equal, infix "<"
--	end
--feature {Any}
--	markedConst : Boolean is False
--end -- class LocalAttrDeclarationDescriptor

--class DetachedUnitAttributeDeclarationDescriptor
--inherit
--	UnitAttrDescriptor
--		redefine
--			out
--	end
--create
--	init
--feature {Any}		
--	markedRigid: Boolean is False
--	markedConst: Boolean is False
--	type: AttachedTypeDescriptor
--	init (isO, isF: Boolean; aName: like name; aType: like type; anAssigner: like assigner) is
--	require
--		name_not_void: aName /= Void
--		type_not_void: aType /= Void
--	do
--		isOverriding := isO
--		isFinal := isF
--		name := aName
--		type := aType
--		assigner := anAssigner
--	end -- init
--	out: String is
--	do
--		Result := memberOut
--		Result.append_string (Precursor)
--		--Result.append_string (name + ": ?" + type.out)
--		Result.append_string (": ?" + type.out)
--		if assigner /= Void then
--			Result.append_character (' ')
--			Result.append_string (assigner.out)
--		end -- if
--	end -- out
--	cutImplementation is
--	do
--		if assigner /= Void then
--			assigner.cutImplementation
--		end -- if
--	end -- cutImplementation
--	generate (cg: CodeGenerator) is
--	do
--	end -- generate
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	do
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- is_invalid
--
--invariant
--	type_not_void: type /= Void
--end -- class DetachedUnitAttributeDeclarationDescriptor

--class DetachedLocalAttributeDeclarationDescriptor
--	-- LocalAttributeCreation  => LocalAttributeNamesList ":" "?" UnitTypeDescriptor
--	-- LocalAttributeNamesList => [var | rigid] Identifier {","[var | rigid] Identifier}
--inherit
--	LocalAttrDeclarationDescriptor
--		redefine
--			out
--	end
--create
--	init
--feature {Any}		
--	markedRigid: Boolean is False
--	markedVar: Boolean is False
--	--type: AttachedTypeDescriptor
--	type: DetachableTypeDescriptor
--
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	local
--		useConst: Sorted_Array [UnitTypeNameDescriptor]
--		stringPool: Sorted_Array [String]
--		typePool: Sorted_Array[TypeDescriptor]	
--	do
--	useConst := context.useConst
--	stringPool := context.stringPool
--	typePool := context.typePool
--		-- do nothing so far
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- isInvalid
--	generate (cg: CodeGenerator) is
--	do
--		-- do nothing so far
--	end -- generate
--
--	init (aName: like name; aType: like type) is
--	require
--	do
--		name := aName
--		type := aType
--	end -- init
--	out: String is
--	do
--		Result := Precursor
--		-- Result.append_string (name + ": " + type.out)
--		Result.append_string (": " + type.out)
--	end -- out
--end -- class DetachedLocalAttributeDeclarationDescriptor

--class AttachedLocalAttributeDeclarationDescriptor
---- LocalAttributeCreation:
----(LocalAttributeNamesList ([‚Äú:‚Äù Type] is Expression [NewLine])|(‚Äú:‚Äù ‚Äú?‚Äù AttachedType))
----|
----(‚Äú(‚Äú LocalAttributeNamesList ‚Äú)‚Äù is Expression  [NewLine])
----|
----(LocalAttributeNamesList‚Äú:‚ÄùAttachedType)
---- LocalAttributeNamesList: [var|rigid] Identifier {‚Äú,‚Äù[var|rigid] Identifier}
--
--inherit
--	LocalAttrDeclarationDescriptor
--		redefine
--			out, setFlags
--	end
class LocalAttrDeclarationDescriptor
inherit
	EntityDeclarationDescriptor
		redefine
			--out, 
			setFlags
	end
	StatementDescriptor
		undefine
			is_equal, infix "<"
	end
create
	init, init_for_search
feature {Any}		
	markedConst : Boolean is False
	markedVar: Boolean
	markedRigid: Boolean
	--type: TypeDescriptor -- AttachedTypeDescriptor
	--expr: ExpressionDescriptor -- if expr is Void then type should have no init or init with no parameters

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	--setFlags (tmpDsc: TemporaryLocalAttributeDescriptor) is
	setFlags (tmpDsc: like Current) is
	do
		markedVar:= tmpDsc.markedVar
		markedRigid:= tmpDsc.markedRigid
	end -- setFlags

	init_for_search (mv, mr: Boolean; aName: like name) is
	do
		init (mv, mr, aName, Void, Void)
	end -- init_for_search
	
	init (mv, mr: Boolean; aName: like name; aType: like type; ie: like expr) is
	require
		non_void_name: aName /= Void
		--consistent_expr_and_type : ie = Void implies aType /= Void
		consistent_flags: mv implies not mr and then mr implies not mv
	do
		markedVar := mv
		markedRigid	:= mr
		name	:= aName
		type	:= aType
		expr:= ie
	end -- init

	--out: String is
	--do
	--	Result := Precursor
	--	--Result := ""
	--	--if markedVar then
	--	--	Result.append_string ("var ")		
	--	--end -- if
	--	--if markedRigid then
	--	--	Result.append_string ("rigid ")
	--	--end -- if
	--	--Result.append_string (name)
	--	--if type /= Void then
	--	--	Result.append_character (':')
	--	--	Result.append_character (' ')
	--	--	Result.append_string (type.out)
	--	--end -- if
	--	--if expr /= Void then
	--	--	Result.append_string (" is ")
	--	--	Result.append_string (expr.out)
	--	--end -- if
	--end -- out
--invariant
--	consistent: not (type = Void and expr = Void)
end -- class LocalAttrDeclarationDescriptor
--end -- class AttachedLocalAttributeDeclarationDescriptor

--class TemporaryLocalAttributeDescriptor
--inherit
--	LocalAttrDeclarationDescriptor
--		redefine
--			out
--	end
--create
--	init
--feature {Any}
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	local
--	--	useConst: Sorted_Array [UnitTypeNameDescriptor]
--	--	stringPool: Sorted_Array [String]
--	--	typePool: Sorted_Array[TypeDescriptor]	
--	do
--	--useConst := context.useConst
--	--stringPool := context.stringPool
--	--typePool := context.typePool
--		-- do nothing so far
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- isInvalid
--	generate (cg: CodeGenerator) is
--	do
--		-- do nothing so far
--	end -- generate
--
--	out: String is
--	do
--		Result := Precursor
--		--if markedVar then
--		--	Result := "var " + name
--		--elseif markedRigid then
--		--	Result := "rigid " + name
--		--else
--		--	Result := "" + name
--		--end -- if
--		Result.append_string (": <Type>")
--	end -- out
--	type: TypeDescriptor is do end
--	expr: ExpressionDescriptor is do end
--	markedVar: Boolean
--	markedRigid: Boolean
--
--	init (isV, isR: Boolean; aName: like name) is
--	require
--		name_not_void: aName /= Void
--	do
--		markedVar := isV
--		markedRigid := isR
--		name := aName
--	end -- init
--end -- class TemporaryLocalAttributeDescriptor

--class TemporaryUnitAttributeDescriptor						
--inherit
--	UnitAttrDescriptor
--		redefine
--			out
--	end
--create
--	init
--feature {Any}
--	out: String is
--	do
--		Result := Precursor
--		--if markedConst then
--		--	Result := "const " + name
--		--elseif markedRigid then
--		--	Result := "rigid " + name
--		--else
--		--	Result := "" + name
--		--end -- if
--		Result.append_string (": <Type>")
--	end -- out
--	type: TypeDescriptor is do end
--	expr: ExpressionDescriptor is do end
--	markedConst: Boolean
--	markedRigid: Boolean
--	init (isC, isR: Boolean; aName: like name) is
--	require
--		name_not_void: aName /= Void
--	do
--		markedConst := isC
--		markedRigid := isR
--		name := aName
--	end -- init
--	cutImplementation is do end
--	generate (cg: CodeGenerator) is do end -- generate
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is do end -- is_invalid
--
--end -- class TemporaryUnitAttributeDescriptor
--class AttachedUnitAttributeDeclarationDescriptor
--	-- UnitAttributeDeclaration:
--	-- ( [const|rigid] Identifier {"," [const|rigid] Identifier} ":" Type)
--	-- |
--	-- ( [const|rigid] Identifier [":" AttachedType] is ConstantExpression) 
--	-- |
--	-- (Identifier ":" Type rtn ":=" [[ Parameters] HyperBlock ] )
--inherit
--	UnitAttrDescriptor
--		redefine
--			out
--	end
--create
--	init
--feature {Any}	
--	markedConst: Boolean
--	markedRigid: Boolean
--	type: TypeDescriptor
--	expr: ExpressionDescriptor
--
--	
--	generate (cg: CodeGenerator) is
--	do
--	end -- generate
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	do
--debug
--	o.putLine ("Validity check for: " + out)
--end -- debug
--	end -- is_invalid
--		
--	
--	cutImplementation is
--	do
--		expr := Void
--		if assigner /= Void then
--			assigner.cutImplementation
--		end -- if
--	end -- cutImplementation
--	
--	init (isO, isF, mc, mr: Boolean; aName: String; aType: like type; a: like assigner; ie: like expr) is
--	require
--		non_void_name: aName /= Void
--		consistent_expr_and_type : ie = Void implies aType /= Void
--		consistent_flags: mc implies not mr and then mr implies not mc
--	do
--		isOverriding := isO
--		isFinal := isF
--		markedConst := mc
--		markedRigid	:= mr
--		name	:= aName
--		type	:= aType
--		assigner:= a
--		expr:= ie
--	end -- init
--	out: String is
--	do
--		Result := memberOut
--		Result.append_string (Precursor)
--		-- Result.append_string (name)
--		if type /= Void then
--			Result.append_character (':')
--			Result.append_character (' ')
--			Result.append_string (type.out)
--		end -- if
--		if expr /= Void then
--			Result.append_string (" is ")
--			Result.append_string (expr.out)
--		end -- if
--		if assigner /= Void then
--			Result.append_character (' ')
--			Result.append_string (assigner.out)
--		end -- if
--	end -- out
--invariant
--	-- consistent_expr_and_type: expr = Void implies type /= Void -- cutImplementation may violate it
--end -- class AttachedUnitAttributeDeclarationDescriptor

-------------- Assigners --------------------
deferred class AttributeAssignerDescriptor
-- rtn ":="[ [Parameters] HyperBlockDescriptor ]
inherit
	Any
		undefine
			out
	end
feature
	cutImplementation is
	deferred
	end 
end -- class AttributeAssignerDescriptor
class DefaultAttributeAssignerDescriptor
inherit
	AttributeAssignerDescriptor
	end
feature {Any}
	out: String is
	do
		Result := "rtn :="
	end -- out
	cutImplementation is
	do
		-- do nothing
	end -- cutImplemenattion
end -- class DefaultAttributeAssignerDescriptor
class CustomAttributeAssignerDescriptor
inherit
	AttributeAssignerDescriptor
	end
create	
	init
feature {Any}
	-- "("ParameterDescriptor {";" ParameterDescriptor}")"
	parameters: Array [ParameterDescriptor]
	body: HyperBlockDescriptor
	init (p: like parameters; b: like body) is 
	require
		parameters_not_void: p /= Void
		body_not_void: b /= Void
	do
		parameters	:= p
		body		:= b
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := "rtn :="
		n := parameters.count
		if n > 0 then
			from
				Result.append_string (" (")
				i := 1
			until
				i > n
			loop
				Result.append_string (parameters.item (i).out)
				if i < n then
					Result.append_string ("; ")
				end -- if
				i := i + 1
			end -- loop
			Result.append_character (')')
		end -- if
		if body = Void then
			Result.append_string (" none")
		else
			Result.append_string (body.out)
		end -- if
	end -- out
	cutImplementation is
	do
		body := Void
	end -- cutImplemenattion
invariant
	parameters_not_void: parameters /= Void
	--body_not_void: body /= Void
end -- class CustomAttributeAssignerDescriptor
---------------- Entities end   -----------------

------------------- Expressions --------------
deferred class ExpressionDescriptor
--29 IfExpression | MemberCallOrCreation | Expression Operator Expression | Operator Expression | Constant | TypeOfExpression | OldExpression
--    | RangeExpression | LambdaExpression | TupleExpression | RefExpression | ì(îExpressionì)î{CallChain}
inherit	
	TypeOrExpressionDescriptor
	end
	SourcePosition
		undefine	
			is_equal, out
	end
feature{Any}
	getOrder: Integer is
-- 0. All other
-- 1. not, ~, /=, =, ^
-- 2. *, /, \, and, &
-- 3. +, -, or, |
	do
		-- Result := 10
	end -- getOrder
	getExternalName: String is
	do
		Result := "<expression>"
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	getFactualGenericExternalName (pos: Integer): String is
	do
		if semType = Void then
-- not_implemened_yet !!!!		
		else
			Result := semType.getExternalName
		end -- if
	end -- getFactualGenericExternalName

	--is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	--require	
	--	non_void_context: context /= Void
	--deferred
	--end -- isInvalid	
	
	semType: UnitTypeCommonDescriptor
	
end -- class ExpressionDescriptor

deferred class TypeOfExpressionDescriptor
inherit
	ExpressionDescriptor
	end
feature {Any}
	expDsc: ExpressionDescriptor
invariant
	non_void_expression: expDsc /= Void
end -- class TypeOfExpressionDescriptor

class IsDetachedDescriptor
inherit
	TypeOfExpressionDescriptor
	end
create
	init
feature {Any}
	init (e: like expDsc) is
	require
		non_void_expression: e /= Void
	do
		expDsc := e
	end -- init
	out: String is
	do
		Result := expDsc.out + " is ?"
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := expDsc.is_equal (other.expDsc)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expDsc < other.expDsc
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if expdsc.isInvalid (context, o) then
			Result := True
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
end -- class IsDetachedDescriptor
class IsAttachedDescriptor
inherit
	TypeOfExpressionDescriptor
	--IsDetachedDescriptor
		--rename
		--	init as detached_init
		--export
		--	{None} detached_init
		--redefine
		--	out, sameAs, lessThan
	end
create
	init
feature {Any}
	typeDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
	init (e: like expDsc; t: like typeDsc) is
	require
		non_void_expression: e /= Void
	do
		-- detached_init (e)
		expDsc := e
		typeDsc := t
	end -- init
	out: String is
	do
		Result := expDsc.out + " is " + typeDsc.out
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := expDsc.is_equal (other.expDsc) and then typeDsc.is_equal (other.typeDsc)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expDsc < other.expDsc
		if not Result and then typeDsc.is_equal (other.typeDsc) then
			Result := typeDsc < other.typeDsc
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if expdsc.isInvalid (context, o) then
			Result := True
		end -- if
		if typeDsc.isInvalid (context, o) then
			Result := True
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
invariant
	non_void_type: typeDsc /= Void
end -- class IsAttachedDescriptor

class ForcedExpressionDescriptor
inherit
	ExpressionDescriptor
	end
create
	init
feature
	forcedType: UnitTypeCommonDescriptor
	realExpr: ExpressionDescriptor
	init (ft: like forcedType; expr: like realExpr) is
	require
		forcedType_not_void: ft /= Void
		realExpr_not_void: expr /= Void
	do
		forcedType := ft
		realExpr := expr
	end -- init
	out: String is
	do
		Result := "{" + forcedType.out + "} " + realExpr.out
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := forcedType.is_equal (other.forcedType) and then realExpr.is_equal (other.realExpr)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := forcedType < other.forcedType
		if not Result and then forcedType.is_equal (other.forcedType) then
			Result := realExpr < other.realExpr
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	forcedType_not_void: forcedType /= Void
	realExpr_not_void: realExpr /= Void
end -- class ForcedExpressionDescriptor

class ParenthedExpressionDescriptor
inherit
	ExpressionDescriptor
	end
create
	init
feature
	expr: ExpressionDescriptor
	init (e: like expr) is
	require
		non_void_expression: e /= Void
	do
		expr := e
	end -- init
	out: String is
	do
		Result := "(" + expr.out + ")"
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expr < other.expr
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	non_void_expression: expr /= Void
end -- class ParenthedExpressionDescriptor

---------
-- class InitDescriptor
-- inherit
-- 	CallDescriptor
-- 		redefine
-- 			sameAs, lessThan
-- --	ExpressionDescriptor
-- --	end
-- --	EntityDescriptor
-- --		undefine
-- --			is_equal
-- --	end
-- create
-- 	init
-- feature
-- 	unitName: String
-- 	init (aName: like unitName) is
-- 	require
-- 		non_void_unit_name: aName /= Void
-- 	do
-- 		unitName := aName
-- 	end -- init
-- 	out: String is do Result :=  "" + unitName end
-- 	sameAs (other: like Current): Boolean is
-- 	do
-- 		Result := unitName.is_equal (other.unitName)
-- 	end -- sameAs
-- 	lessThan (other: like Current): Boolean is
-- 	do
-- 		Result := unitName < other.unitName
-- 	end -- theSame
-- 	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
-- 	local
-- 		--useConst: Sorted_Array [UnitTypeNameDescriptor]
-- 		--stringPool: Sorted_Array [String]
-- 		--typePool: Sorted_Array[TypeDescriptor]	
-- 		-- notValid: Boolean
-- 	do
-- 	--useConst := context.useConst
-- 	--stringPool := context.stringPool
-- 	--typePool := context.typePool
-- 		-- do nothing so far
-- 	end -- isInvalid
-- --feature {ExpressionDescriptor}
-- --	weight: Integer is -2
-- end -- class InitDescriptor
---------



class OldDescriptor
inherit
	CallDescriptor
		redefine
			sameAs, lessThan
	end
feature
	out: String is do Result := "old" end

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	sameAs (other: like Current): Boolean is
	do
		Result := True
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
	end -- theSame
end -- class OldDescriptor
class ThisDescriptor
inherit
	--CallDescriptor
	--	redefine
	--		sameAs, lessThan
	ExpressionDescriptor
	end
	EntityDescriptor
		undefine
			is_equal
	end
feature
	out: String is do Result := "this" end
	sameAs (other: like Current): Boolean is
	do
		Result := True
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

end -- class ThisDescriptor
class ReturnDescriptor
inherit
	--CallDescriptor
	--	redefine
	--		sameAs, lessThan
	ExpressionDescriptor
	end
feature {Any}
	out: String is do Result := "return" end
	sameAs (other: like Current): Boolean is
	do
		Result := True
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
end -- class ReturnDescriptor
deferred class EntityDescriptor
inherit
	ExpressionDescriptor
	end
end -- class EntityDescriptor

class IdentifierDescriptor
inherit
	--CallDescriptor
	--ConstExpressionDescriptor
	ExpressionDescriptor
		redefine
			getExternalName
		--	sameAs, lessThan
	end
	ConstObjectDescriptor
		undefine
			is_equal, infix "<"
	end
	EntityDescriptor
		undefine
			is_equal, getExternalName
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature {Any}	
	--name: String

	init (s: String) is
	require
		identifier_name_not_void: s /= Void
	do
		name := s
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
	end -- theSame
	out: String is
	do
		create Result.make_from_string (name)
	end 
	getExternalName: String is
	do
		Result := out
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	markedConst, markedRigid, MarkedVar: Boolean is False
	--type: TypeDescriptor is once end
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
		pos: Integer
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	--useConst := context.useConst
	stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
		pos := stringPool.seek (name)
		check
			identifer_name_not_registered_in_the_pool: pos > 0
		end -- check
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

end -- class IdentifierDescriptor

class GenericIdentifierDescriptor
inherit
	IdentifierDescriptor
		rename init as non_actual_init
		export {None} non_actual_init 
	end
create
	init
feature
	generics: Array [TypeOrExpressionDescriptor]
	init (n: like name; g: like generics) is
	require
		non_void_name: n /= Void
		non_void_generics: g /= Void
	do
		name := n
		generics := g
	end -- init
end -- class GenericIdentifierDescriptor 

class TupleExpressionDescriptor
inherit
	ExpressionDescriptor
	end
create
	init
feature
	tuple: Array [ExpressionDescriptor]
	init (t: like tuple) is
	do
		if t = Void then
			create tuple.make (1, 0)
		else
			tuple := t
		end -- if
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := "("
		from
			i := 1
			n := tuple.count
		until
			i > n
		loop
			Result.append_string (tuple.item (i).out)
			if i < n then
				Result.append_string (", ")
			end -- if
			i := i + 1
		end -- loop
		Result.append_character(')')
	end -- out
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		from
			Result := True
			i := 1
			n := tuple.count
		until
			i > n
		loop
			if tuple.item (i).is_equal (other.tuple.item (i)) then
				i := i + 1
			else
				Result := False
				i := n + 1
			end -- if
		end -- loop
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := tuple.count
		m := other.tuple.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if tuple.item (i) < other.tuple.item (i) then
					Result := True
					i := n + 1
				elseif tuple.item (i).is_equal (other.tuple.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		--notValid: Boolean
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- Every tuple element should be a vlaid expression
		from
			i := 1
			n := tuple.count
		until
			i > n
		loop
			if tuple.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	tuple_not_void: tuple /= Void
end -- class TupleExpressionDescriptor


class RefExpressionDescriptor
-- ref Expression
inherit
	ExpressionDescriptor
	end
create
	init
feature {Any}	
	expr: ExpressionDescriptor
	out: String is
	do
		Result := "ref " + expr.out
	end -- out
	init (e: like expr) is
	do
		expr := e
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expr < other.expr
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
		if expr.isInvalid (context, o) then
			Result := True
		end -- if
		if not Result then
			-- ref expr
			-- type of expr should be of the value type !!!
			-- not_implemented_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

invariant
	non_void_expression: expr /= Void
end -- class RefExpressionDescriptor

deferred class LambdaExpression
-- (rtn Identifier [Signature])|InlineLambdaExpression
-- InlineLambdaExpression  : [pure|safe] rtn [Parameters] [ì:î Type]
-- 	( [RequireBlock] InnerBlockDescriptor | foreign [EnsureBlock] [end] )|(ì=>îExpression )
inherit
	ExpressionDescriptor
	end
feature {Any}	
end -- class LambdaExpression

class LambdaFromRoutineExpression
-- rtn Identifier [Signature]
inherit
	LambdaExpression
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature {Any}
	--name: String
	signature: SignatureDescriptor
	out: String is
	do
		Result := "rtn " + name
		if signature /= Void then
			Result.append_string (signature.out)
		end -- if
	end -- out	
	init (n: like name; s: like signature) is
	require
		name_not_void: n /= Void
	do
		name := n
		signature := s
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name)
		if Result then
			Result := signature = other.signature
			if not Result and then signature /= Void and then other.signature /= Void then
				Result := signature.is_equal(other.signature)
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name)  then
			Result := signature = Void and then other.signature /= Void
			if not Result and then signature /= Void and then other.signature /= Void then
				Result := signature < other.signature
			end -- if
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
		-- name: String
		-- it should be a valid routine name
		-- signature: SignatureDescriptor
		if signature /= Void then
			if signature.isInvalid (context, o) then
				Result := True
			end -- if
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	--name_not_void: name /= Void
end -- class LambdaFromRoutineExpression

class InlineLambdaExpression
-- [pure|safe] rtn [Parameters] [":" Type] ( [RequireBlock] InnerBlockDescriptor | foreign [EnsureBlock] [end] )|("=>" Expression )
inherit
	LambdaExpression
	end
	UnitRoutineDeclarationDescriptor
		rename	
			init as unitRoutineInit
		export 
			{None} unitRoutineInit
		undefine
			is_equal, infix "<", sameAs, lessThan
		redefine	
			is_invalid, generate
	end
create
	init
feature {Any}
	init (p: like parameters; t: like type; pre: like preconditions; isF: Boolean; b: like innerBlock; e: like expr; post: like postconditions) is
	do
		unitRoutineInit (False, False, False, False, "<>", Void, p, t, Void, Void, pre, isF, False,  b, e, post)
	end -- init
	sameAs (other: like Current): Boolean is
	once
print ("InlineLambdaExpression.sameAs not_implemented_yet%N")
	end -- sameAs
	lessThan (other: like Current): Boolean is
	once
print ("InlineLambdaExpression.lessThan not_implemented_yet%N")
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

end -- class InlineLambdaExpression

     
class InRangeExpression
-- Expr in RangeExpressionDescriptor
inherit
	ExpressionDescriptor
	end
create
	init
feature {Any}	
	expr: ExpressionDescriptor
	range: ExpressionDescriptor
	init (e: like expr; r: like range) is
	require
		non_void_expr: e /= Void
		non_void_range: r /= Void
	do
		expr  := e
		range := r
	end -- init
	out: String is
	do
		Result := expr.out + " in " + range.out
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr) and then range.is_equal (other.range)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expr < other.expr
		if not Result and then expr.is_equal (other.expr) then
			Result := range < other.range
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

invariant
	non_void_expr: expr /= Void
	non_void_range: range /= Void
end -- class InRangeExpression

class RangeExpressionDescriptor
-- RangeExpression : Expression ["{"OperatorName ConstantExpression "}"] ".."Expression
inherit
	ExpressionDescriptor
	end
create
	init
feature {Any}	
	left, right: ExpressionDescriptor
	operator: String
	expr: ExpressionDescriptor
	out: String is
	do
		Result := ""
		Result.append_string (left.out)
		if operator /= Void then
			Result.append_character(' ')
			Result.append_character('{')
			Result.append_string (operator.out)
			Result.append_character(' ')
			Result.append_string (expr.out)
			Result.append_character('}')
		end -- if
		Result.append_string (" .. ")
		Result.append_string (right.out)
	end -- out
	
	init (l: like left; o: like operator; e: like expr; r: like right) is
	require
		left_expr_not_void: l /= Void
		right_expr_not_void: r /= Void
		consistent: o /= Void implies e /= Void
	do
		left := l
		right := r
		operator:= o
		expr:= e
	end -- init
	sameAs (other: like Current): Boolean is
	do
		--Result := left.is_equal (other.left) and then right.is_equal (other.right) and then 
		--(operator = Void and then other.operator = Void or else operator /= Void and then other.operator /= Void implies expr.is_equal (other.expr))
		Result := left.is_equal (other.left) and then right.is_equal (other.right)
		if Result then
			if operator = Void and then other.operator = Void then
			elseif operator /= Void and then other.operator /= Void then
				Result := expr.is_equal (other.expr)
			else
				Result := False
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := left < other.left
		if not Result and then left.is_equal (other.left) then
			Result := right < other.right
			if not Result and then right.is_equal (other.right) then
				Result := operator = Void and then other.operator /= Void
				if  not Result and then operator /= Void and then other.operator /= Void then
					Result := operator < other.operator
					if not Result and then operator.is_equal (other.operator) then
						Result := expr < other.expr
					end -- if
				end -- if
			end -- if
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	left_expr_not_void: left /= Void
	right_expr_not_void: right /= Void
	consistent: operator /= Void implies expr /= Void
end -- class RangeExpressionDescriptor

class OldExpressionDescriptor
-- old Expression 
inherit
	ExpressionDescriptor
	end
create
	init
feature {Any}	
	expr: ExpressionDescriptor
	out: String is
	do
		Result := "old " + expr.out
	end
	init (e: like expr) is
	require
		expr_not_void: e /= Void
	do
		expr := e
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expr < other.expr
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

invariant
	expr_not_void: expr /= Void
end -- class OldExpressionDescriptor

--class TupleExpressionDescriptor
---- "("[TupleElement {"," TupleElement}]")"
--inherit
--	ExpressionDescriptor
--	end
--create
--	init
--feature {Any}	
--	tuples: Array [TupleElement]
--	out: String is
--	local
--		i, n: Integer
--	do
--		from
--			Result := " ("
--			i := 1
--			n := tuples.count
--		until
--			i > n
--		loop
--			Result.append_string (tuples.item(i).out)
--			if i < n then
--				Result.append_string (", ")
--			end -- if
--			i := i + 1
--		end -- loop
--		Result.append_character (')')
--	end -- out
--	sameAs (other: like Current): Boolean is
--	local
--		i, n: Integer
--	do
--		from
--			i := 1
--			n := tuples.count
--			Result := True
--		until
--			i > n
--		loop
--			if tuples.item(i).is_equal (other.tuples.item(i)) then
--				i := i + 1
--			else	
--				Result := False
--				i := n + 1
--			end -- if
--		end -- loop
--	end -- sameAs
--	lessThan (other: like Current): Boolean is
--	local
--		i, n, m: Integer
--	do
--		n := tuples.count
--		m := other.tuples.count
--		Result := n < m
--		if not Result and then n = m then
--			from
--				i := 1
--			until
--				i > n
--			loop
--				if tuples.item(i) < other.tuples.item(i) then
--					i := n + 1
--					Result := True
--				elseif tuples.item(i).is_equal (other.tuples.item(i)) then
--					i := i + 1
--				else	
--					i := n + 1
--				end -- if
--			end -- loop
--		end -- if
--	end -- lessThan
--	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
--	local
--		useConst: Sorted_Array [UnitTypeNameDescriptor]
--		stringPool: Sorted_Array [String]
--		typePool: Sorted_Array[TypeDescriptor]	
--		-- notValid: Boolean
--	do
--	useConst := context.useConst
--	stringPool := context.stringPool
--	typePool := context.typePool
--		-- do nothing so far
--	end -- isInvalid
----feature {ExpressionDescriptor}
----	weight: Integer is -14
--end
--class TupleElement 
---- Expression| Parameter
--inherit	
--	Any
--		redefine
--			out
--	end
--create
--	init
--feature
--	expr: ExpressionDescriptor
--	param: ParameterDescriptor
--	out: String is
--	do
--		if expr = Void then
--			Result := param.out
--		else
--			Result := expr.out
--		end -- if
--	end -- out
--	init(e: like expr; p : like param) is
--	require
--		consistent_tuple_element: not (e = Void and then p = Void)
--	do
--		expr  := e
--		param := p
--	end -- init	
--invariant
--	consistent_tuple_element: not (expr = Void and then param = Void)
--end -- class TupleElement

class TypeOfExpression
-- Expression is "?"| UnitTypeDescriptor
inherit
	ExpressionDescriptor
	end
create
	init
feature {Any}	
	expr: ExpressionDescriptor
	type: UnitTypeDescriptor
	out: String is
	do
		if type = Void then
			Result := expr.out + " is ?"
		else
			Result := expr.out + " is " + type.out
		end -- if
	end
	init (e: like expr; t: like type) is
	require
		expr_not_void: e /= Void
	do
		expr := e
		type := t
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr)
		if Result then
			Result := type = other.type
			if not Result and then type /= Void and then other.type /= Void then
				Result := type.is_equal (other.type)
			end 
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expr < other.expr
		if not Result and then expr.is_equal (other.expr) and then type /= Void and then other.type /= Void  then
			Result := type < other.type
		end -- if
	end -- sameAs
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

invariant
	expr_not_void: expr /= Void
end -- class TypeOfExpression

-- Operator: OperatorName|in

class ConstantDescriptor
-- [UnitTypeNameDescriptor "."] CharacterConstant |IntegerConstant |RealConstant |StringConstant |Identifier  
inherit
	ConstObjectDescriptor
		undefine
			is_equal, infix "<"
	end
	--CallDescriptor
	--	redefine
	--		sameAs, lessThan
	--ConstExpressionDescriptor
	ExpressionDescriptor
	end
create
	init
feature	{Any}
--	unitPrefix: UnitTypeNameDescriptor
	token: Integer
	value: Any
	
	negate is
	local
		ir: Integer_Ref
		rr: Real_Ref
	do
		ir ?= value
		if ir = Void then
			rr ?= value
			if rr /= Void then
				value := - rr.item
			end -- if
		else
			value := - ir.item
		end -- if
	end -- negate
	
--	init (up: like unitPrefix; t: Integer; v: like value) is
	init (t: Integer; v: like value) is
	require
		non_void_value: v /= Void
	do
--		unitPrefix:= up
		token:= t
		value:= v
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := token = other.token and then value.is_equal (other.value)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local	
		aString: String
		anInteger: Integer_Ref
		aReal: Real_Ref
		aChar: Character_Ref
		otherString: String
		otherInteger: Integer_Ref
		otherReal: Real_Ref
		otherChar: Character_Ref
	do
		Result := token < other.token
		if not Result and then token = other.token then
			aString ?= value
			anInteger ?= value
			aReal ?= value
			aChar ?= value
			if aChar /= Void then
				otherChar ?= other.value
				Result := aChar.item < otherChar.item
			elseif  anInteger /= Void then
				otherInteger ?= other.value
				Result := anInteger.item < otherInteger.item
			elseif aReal /= Void then
				otherReal ?= other.value
				Result := aReal.item < otherReal.item
			elseif aString /= Void then
				otherString ?= other.value
				Result := aString < otherString
			end -- if
		end -- if
	end -- lessThan	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
		-- Always valid !!!
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	local
		aString: String
		aChar: Character_Ref
	do
		aString ?= value
		if aString /= Void then
			Result := "%"" + aString + "%""
		else
			aChar ?= value
			if aChar /= Void then
				Result := "`" + value.out + "`"
			else
				Result := value.out
			end -- if
		end -- if
	end -- out
invariant
	non_void_value: value /= Void
	-- valid_token: token = scanner.string_const_token or else
	--	token = scanner.character_const_token or else
	--	token = scanner.integer_const_token or else
	--	token = scanner.real_const_token or else
	--	token = scanner.string_const_token or else
	--	token = scanner.identifier_token
end -- class ConstantDescriptor

class CallChainElement
-- CallChain: "."Identifier [ Arguments ]
-- Arguments: "(" [Expression{"," Expression}] ")"
inherit
	Comparable
		redefine
			out, is_equal
	end
	BuildServer
		redefine
			out, is_equal
	end
create
	init
feature {Any}
	identifier: String
	arguments: Array [ExpressionDescriptor]
	init (id: like identifier; args: like arguments) is
	require
		identifier_not_void: id /= Void
	do
		identifier := id
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
	end -- init
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	
	getOrder: Integer is
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
		when '>', '<' then
			Result := 3
		else
			-- Result := 10
		end -- inspect
	end -- getOrder

	out: String is
	local
		i, n: Integer
	do
		Result:= "."
		Result.append_string (identifier)
		n := arguments.count
		if n > 0 then
			from
				Result.append_string (" (")
				i := 1
			until
				i > n
			loop
				Result.append_string (arguments.item(i).out)
				if i < n then
					Result.append_character(',')
					Result.append_character(' ')
				end -- if
				i := i + 1
			end -- loop
			Result.append_character (')')
		end -- if
	end -- out
	is_equal (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := identifier.is_equal (other.identifier)
		if Result then
			n := arguments.count
			Result := n = other.arguments.count 
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if arguments.item(i).is_equal (other.arguments.item(i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := identifier < other.identifier
		if not Result and then identifier.is_equal (other.identifier) then
			n := arguments.count
			m := other.arguments.count
			Result := n < m			
			if not Result and then n = m then
				from
					i := 1
				until
					i > n
				loop
					if arguments.item(i) < other.arguments.item(i) then
						Result := True
						i := n + 1
					elseif arguments.item(i).is_equal (other.arguments.item(i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- infix "<"
invariant
	identifier_not_void: identifier /= Void
	arguments_not_void: arguments /= Void
end -- class CallChainElement

class QualifiedConstantDescriptor
inherit
	ConstExpressionDescriptor
	end
	CallDescriptor
		redefine
			sameAs, lessThan
	end
create	
	init
feature
	exprDsc: ExpressionDescriptor
	constDsc: ConstantDescriptor

	init (ceDsc: like exprDsc; cDsc: like constDsc) is
	require
		non_void_expr: ceDsc /= Void
		non_void_const: cDsc /= Void
	do
		exprDsc	:= ceDsc
		constDsc:= cdsc
		create callChain.make (1, 0)
	end -- init

	out: String is
	do
		Result := exprDsc.out + "." + constDsc.out
	end -- out

	sameAs (other: like Current): Boolean is
	do
		Result := exprDsc.is_equal (other.exprDsc) and then constDsc.is_equal (other.constDsc)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := exprDsc < other.exprDsc
		if not Result and then exprDsc.is_equal (other.exprDsc) then
			Result := constDsc < other.constDsc
		end -- if	
	end -- lessThan
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid

	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	non_void_expr: exprDsc /= Void
	non_void_const: constDsc /= Void
end -- class QualifiedConstantDescriptor

deferred class CallDescriptor
-- WritableCall |  (this {CallChain})
-- WritableCall: (  (Identifier ["."Identifier])| (old ["{"UnitTypeName"}"]) [Arguments] )| return {CallChain}
inherit
	StatementDescriptor
		undefine
			is_equal, infix "<"
	end
	--ConstExpressionDescriptor
	ExpressionDescriptor
	end
feature {Any}
	callChain: Array [CallChainElement]	
	isWritable: Boolean is
	do
		Result := True
	end -- isWritable
	setCallChain (cc: like callChain) is
	do
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if
	end -- setCallChain
	outCallChain: String is
	local
		i, n: Integer		
	do
		Result := ""
		from
			i := 1
			n := callChain.count
		until
			i > n
		loop
			Result.append_string (callChain.item (i).out)
			i := i + 1
		end -- loop
	ensure
		Result /= Void
	end -- outCallChain
	theSameCallChain(other: like Current): Boolean is
	require
		other_not_void: other /= Void
	local
		i, n: Integer		
	do
		from
			i := 1
			n := callChain.count
			Result := True
		until
			i > n
		loop
			if callChain.item (i).is_equal (other.callChain.item (i)) then
				i := i + 1
			else
				Result := False
				i := n + 1
			end -- if
		end -- loop
	end -- theSameCallChain
	lessThanCallChain (other: like Current): Boolean is
	require
		other_not_void: other /= Void
	local
		i, n, m: Integer		
	do
		n := callChain.count
		m := other.callChain.count
		Result := n < m
		if not Result and then n = m then 
			from
				i := 1
			until
				i > n
			loop
				if callChain.item (i) < other.callChain.item (i) then
					Result := True
					i := n + 1
				elseif callChain.item (i).is_equal (other.callChain.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- outCallChain
invariant
	non_void_call_chain: callChain /= Void
end -- class CallDescriptor

class WritableTupleDescriptor
-- "("WritableCall {"," WritableCall } ")"
inherit
	CallDescriptor
		redefine
			sameAs, lessThan
	end
create
	init
feature {Any}
	tuple: Array [ExpressionDescriptor]

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	init (t: like tuple; cc: like callChain) is
	require
		tuple_not_void: t /= Void
	do
		tuple := t
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := "("
		from
			n := tuple.count
			i := 1
		until
			i < n
		loop
			Result.append_string (tuple.item(i).out)
			if i < n then
				Result.append_string (", ")
			end -- if
			i := i + 1
		end -- loop
		Result.append_character (')')
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		from
			n := tuple.count
			i := 1
			Result := True
		until
			i < n
		loop
			if tuple.item(i).is_equal (other.tuple.item(i)) then
				i := i + 1
			else
				Result := False
				i := n + 1
			end -- if
		end -- loop
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := tuple.count
		m := other.tuple.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i < n
			loop
				if tuple.item(i) < other.tuple.item(i) then
					Result := True
					i := n + 1
				elseif tuple.item(i).is_equal (other.tuple.item(i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- lessThan

invariant
	tuple_not_void: tuple /= Void
end -- class WritableTupleDescriptor

class ExpressionCallDescriptor
--  "("Expression")"{CallChain}
inherit
	CallDescriptor
		redefine
			sameAs, lessThan, getOrder
	end
create
	init
feature{Any}
	expression: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
		if expression.isInvalid (context, o) then
			Result := True
		end --if
		if callChain /= Void then
			from
				i := 1
				n := callChain.count
			until
				i > n
			loop
				if callChain.item(i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	init (expr: like expression; cc: like callChain) is
	require
		non_void_expression : expr /= Void
	do
		expression := expr
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if	
	end -- init
	getOrder: Integer is
	do
		if callChain.count = 0 then
			-- Result := 10
		else
			Result := callChain.item (1).getOrder		
		end -- if
	end -- getOrder
	out: String is 
	local
		identDsc: IdentifierDescriptor
		thisDsc: ThisDescriptor
		returnDsc: ReturnDescriptor
		constDsc: ConstantDescriptor
	do
		identDsc ?= expression
		if identDsc = Void then
			thisDsc ?= expression
			if thisDsc = Void then
				returnDsc ?= expression
				if returnDsc = Void then
					constDsc ?= expression
					if constDsc = Void then
						Result := "("
						Result.append_string (expression.out)
						Result.append_character(')')
					else
						Result := constDsc.out
					end -- if
				else
					Result := returnDsc.out
				end -- if
			else
				Result := thisDsc.out
			end -- if
		else
			Result := identDsc.out
		end -- if
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := expression.is_equal (other.expression) and then theSameCallChain (other)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expression < other.expression
		if not Result and then expression.is_equal (other.expression) then
			Result := lessThanCallChain (other)
		end -- if
	end -- theSame
invariant
	non_void_expression : expression /= Void
end -- class ExpressionCallDescriptor

class UnqualifiedCallDescriptor
--  Identifier [Arguments] {CallChain}
inherit
	CallDescriptor
		redefine
			sameAs, lessThan
	end
create
	init
feature{Any}
--	target: CallDescriptor
	target: ExpressionDescriptor
	arguments: Array [ExpressionDescriptor]

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if target.isInvalid (context, o) then
			Result := True
		end -- if
		if arguments /= Void then
			from
				n := arguments.count
				i := 1
			until
				i > n
			loop
				if arguments.item (i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop			
		end -- if
		if callChain /= Void then
			from
				n := callChain.count
				i := 1
			until
				i > n
			loop
				if callChain.item (i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop			
		end -- if
		if not Result then
			-- Need to check that target(arguments).classChain combination is valid
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	init (ceDsc: like target; args: like arguments; cc: like callChain) is
	require
		non_void_entity : ceDsc /= Void
	do
		target := ceDsc
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if	
	end -- init
	
	out: String is 
	local
		i, n : Integer
	do
		Result := ""
		Result.append_string (target.out) 
		if arguments /= Void then
			n := arguments.count
			if n > 0 then
				from
					Result.append_string (" (")
					i := 1
				until
					i > n
				loop
					Result.append_string (arguments.item (i).out)
					if i < n then
						Result.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				Result.append_character(')')
			end -- if
		end -- if
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	local
		i, n : Integer
	do
		Result := target.is_equal (other.target)
		if Result then
			if arguments = Void then
				Result := other.arguments = Void
			elseif other.arguments /= Void then
				n := arguments.count
				Result := n = other.arguments.count 
				if Result then
					from
						i := 1
					until
						i > n
					loop
						if arguments.item (i).is_equal (other.arguments.item (i)) then
							i := i + 1
						else
							Result := False
							i := n + 1
						end -- if
					end -- loop
				end -- if
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m : Integer
	do
		Result := target < other.target
		if not Result and then target.is_equal (other.target) then
			if arguments = Void then
				Result := other.arguments /= Void
			elseif other.arguments /= Void then
				n := arguments.count
				m := other.arguments.count
				Result := n < m
				if not Result and then n = m then
					from
						i := 1
					until
						i > n
					loop
						if arguments.item (i) < other.arguments.item (i) then
							Result := True							
							i := n + 1
						elseif arguments.item (i).is_equal (other.arguments.item (i)) then
							i := i + 1
						else
							i := n + 1
						end -- if
					end -- loop
				end -- if
			end -- if
		end -- if
	end -- lessThan
invariant
	non_void_target: target /= Void
end -- class UnqualifiedCallDescriptor

class ConstantCallDescriptor
--  ConstantCall: Constant "."Identifier [Arguments]  {CallChain}
inherit
	CallDescriptor
		redefine
			sameAs, lessThan
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature{Any}
	constDsc: ConstantDescriptor
	--name: String
	arguments: Array [ExpressionDescriptor]

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	init (cd: like constDsc; aName: String; args: like arguments; cc: like callChain) is
	require
		non_void_constant: cd /= Void
		non_void_name: aName /= Void
	do
		constDsc := cd
		name := aName
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if	
	end -- init
	
	out: String is 
	local
		i, n : Integer
	do
		Result := ""
		Result.append_string (constDsc.out)
		Result.append_character('.')
		Result.append_string(name)
		if arguments /= Void then
			n := arguments.count
			if n > 0 then
				from
					Result.append_string (" (")
					i := 1
				until
					i > n
				loop
					Result.append_string (arguments.item (i).out)
					if i < n then
						Result.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				Result.append_character(')')
			end -- if
		end -- if
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	local
		i, n : Integer
	do
		Result := constDsc.is_equal (other.constDsc) and then name.is_equal (other.name)
		if Result then
			if arguments = Void then
				Result := other.arguments = Void
			else
				n := arguments.count
				Result := n = other.arguments.count 
				if Result then
					from
						i := 1
					until
						i > n
					loop
						if arguments.item (i).is_equal (other.arguments.item (i)) then
							i := i + 1
						else
							i := n + 1
							Result := False
						end -- if
					end -- loop
				end -- if
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m : Integer
	do
		Result := constDsc < other.constDsc
		if not Result and then constDsc.is_equal (other.constDsc) then
			Result := name < other.name
			if not Result and then name.is_equal (other.name) then
				if arguments = Void then
					Result := other.arguments /= Void
				else
					n := arguments.count
					m := other.arguments.count
					Result := n < m
					if not Result and then n = m then
						from
							i := 1
						until
							i > n
						loop
							if arguments.item (i) < other.arguments.item (i) then
								i := n + 1
								Result := True
							elseif arguments.item (i).is_equal (other.arguments.item (i)) then
								i := i + 1
							else
								i := n + 1
							end -- if
						end -- loop
					end -- if
				end -- if
			end -- if
		end -- if
	end -- lessThan
invariant
	non_void_constant: constDsc /= Void
	--non_void_identifier : name /= Void
end -- class ConstantCallDescriptor

class QualifiedCallDescriptor
--  Identifier|this|return "."Identifier [Arguments] {CallChain}
inherit
	UnqualifiedCallDescriptor
		rename
			init as UnqualifiedCallInit
		export {None} UnqualifiedCallInit
		redefine
			out, sameAs, lessThan, getOrder, is_invalid, generate
	end
create
	init
feature{Any}
	identifier: String
	init (ceDsc: like target; aName: like identifier; args: like arguments; cc: like callChain) is
	require
		non_void_name: aName /= Void
	do
		identifier := aName
		UnqualifiedCallInit (ceDsc, args, cc)
	end -- init
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if target.isInvalid (context, o) then
			Result := True
		end -- if
		if arguments /= Void then
			from
				n := arguments.count
				i := 1
			until
				i > n
			loop
				if arguments.item (i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop			
		end -- if
		if callChain /= Void then
			from
				n := callChain.count
				i := 1
			until
				i > n
			loop
				if callChain.item (i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop			
		end -- if
		if not Result then
			-- Need to check that target.identifier(arguments).classChain combination is valid
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	
	
	getOrder: Integer is
-- 0. All other
-- 1. not, ~, /=, =, ^
-- 2. *, /, \, and, &
-- 3. +, -, or, |
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
	out: String is
	local
		i, n : Integer
	do
		Result := ""
		Result.append_string (target.out) 
		Result.append_character('.')
		Result.append_string (identifier) 
		if arguments /= Void then
			n := arguments.count
			if n > 0 then
				from
					Result.append_string (" (")
					i := 1
				until
					i > n
				loop
					Result.append_string (arguments.item (i).out)
					if i < n then
						Result.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				Result.append_character(')')
			end -- if
		end -- if
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	local
		i, n : Integer
	do
		Result := target.is_equal (other.target) and then identifier.is_equal (other.identifier)		 
		if Result then
			Result := arguments = other.arguments
			if not Result and then arguments /= Void and then other.arguments /= Void then
				n := arguments.count
				Result := n = other.arguments.count 
				if Result then
					from
						i := 1
					until
						i > n
					loop
						if arguments.item (i).is_equal (other.arguments.item (i)) then
							i := i + 1
						else
							Result := False
							i := n + 1
						end -- if
					end -- loop
				end -- if
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m : Integer
	do
		Result := target < other.target
		if not Result and then target.is_equal (other.target) then
			Result := identifier < other.identifier
			if not Result and then identifier.is_equal (other.identifier) then
				Result := arguments = Void and then other.arguments /= Void 
				if not Result and then arguments /= Void and then other.arguments /= Void then
					n := arguments.count
					m := other.arguments.count
					Result := n < m
					if not Result and then n = m then
						from
							i := 1
						until
							i > n
						loop
							if arguments.item (i) < other.arguments.item (i) then
								Result := True
								i := n + 1
							elseif arguments.item (i).is_equal (other.arguments.item (i)) then
								i := i + 1
							else
								i := n + 1
							end -- if
						end -- loop
					end -- if
				end -- if
			end -- if
		end -- if
	end -- lessThan
invariant
	non_void_identifier : identifier /= Void
end -- class QualifiedCallDescriptor

class ThisCallDescriptor
-- this {CallChain}
inherit
	CallDescriptor
		rename
			setCallChain as init,
			theSameCallChain as theSame,
			lessThanCallChain as lessThan
		redefine
			isWritable
		--	theSame, lessThan
	end
create
	init
feature{Any}
	isWritable: Boolean is False

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	do
		Result := "this" + outCallChain
	end -- out
end -- class ThisCallDescriptor

class ReturnCallDescriptor
-- return {CallChain}
inherit
	CallDescriptor
		rename
			setCallChain as init,
			theSameCallChain as sameAs,
			lessThanCallChain as lessThan
	end
create
	init
feature{Any}
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	out: String is
	do
		Result := "return " + outCallChain
	end -- out
end -- class ReturnCallDescriptor

class RoutineArguments
feature 
	arguments: Array [ExpressionDescriptor]
	setArguments (args: like arguments) is
	do
		arguments := args
	end -- setArguments
	outArguments: String is
	local
		i, n: Integer
	do
		if arguments = Void then
			Result := ""
		else
			n := arguments.count
			if n = 0 then
				Result := ""
			else
				from 
					Result := "("
					i := 1
				until
					i > n
				loop
					Result.append_string (arguments.item (i).out)
					if i < n then
						Result.append_string (", ")
					end -- if
					i := i + 1
				end -- loop
				Result.append_character (')')
			end -- if
		end -- if
	end -- outArguments
feature {RoutineArguments}
	sameArguments (other: like Current): Boolean is
	local
		i, n: Integer
	do
		if arguments = Void then
			Result := other.arguments = Void
		elseif other.arguments /= Void then
			n := arguments.count
			if n = other.arguments.count then
				from 
					Result := True
					i := 1
				until
					i > n
				loop
					if arguments.item (i).is_equal (other.arguments.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- sameArguments
	lessArguments (other: like Current): Boolean is
	local
		i, n: Integer
	do
		if arguments = Void then
			Result := other.arguments /= Void
		elseif other.arguments /= Void then
			n := arguments.count
			Result := n < other.arguments.count
			if not Result and then n = other.arguments.count then
				from 
					i := 1
				until
					i > n
				loop
					if arguments.item (i) < other.arguments.item (i) then
						Result := True
						i := n + 1
					elseif arguments.item (i).is_equal (other.arguments.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- lessArguments
end -- class RoutineArguments

class InitCallDescriptor
inherit
	--CallDescriptor
	--	--rename
	--	--	outCallChain as out, 
	--	--	theSameCallChain as sameAs,
	--	--	lessThanCallChain as lessThan
	--	redefine
	--		sameAs, lessThan
	--end

	ExpressionDescriptor
	end

	StatementDescriptor
		redefine
			sameAs, lessThan
	end
	RoutineArguments
		undefine
			is_equal
		redefine
			out
	end 
create
	init
feature{Any}
	unitTypeDsc: UnitTypeNameDescriptor
	init (unitDsc: like unitTypeDsc; args: like arguments) is
	do
		unitTypeDsc := unitDsc
		arguments := args
	end -- if
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
		useConst := context.useConst
		stringPool := context.stringPool
		typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	out: String is
	do
		Result := unitTypeDsc.out
		Result.append_string (outArguments)
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := unitTypeDsc.is_equal (other.unitTypeDsc) and then sameArguments (other)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := unitTypeDsc < other.unitTypeDsc
		if not Result and then unitTypeDsc.is_equal (other.unitTypeDsc) then
			Result := lessArguments (other)
		end -- if
	end -- lessThan
end -- class InitCallDescriptor

class PrecursorCallDescriptor
-- old ["{"UnitTypeName"}"] [Arguments] {CallChain}
inherit
	CallDescriptor
		redefine
			sameAs, lessThan
	end
	RoutineArguments
		undefine
			is_equal
		redefine
			out
	end 
create
	init
feature{Any}
	unitType: UnitTypeCommonDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	init (ut: like unitType; args: like arguments; cc: like callChain) is
	do
		unitType := ut
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
		if cc = Void then
			create callChain.make (1, 0)
		else
			callChain := cc
		end -- if	
	end -- init
	out: String is
	do
		Result := "old "
		if unitType /= Void then
			Result.append_string (" {")
			Result.append_string (unitType.out)
			Result.append_string ("} ")
		end -- if
		Result.append_string (outArguments)
		Result.append_string (outCallChain)
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := unitType.is_equal (other.unitType) and then sameArguments (other)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := unitType < other.unitType
		if not Result and then unitType.is_equal (other.unitType) then
			Result := lessArguments (other)
		end -- if
	end -- theSame
end -- class PrecursorCallDescriptor

class NewStatementDescriptor
-- new ['{' UnitType '}'] ( identifier | return ) [ Arguments ]
inherit
	StatementDescriptor
	end
	RoutineArguments
		undefine
			is_equal
		redefine
			out
	end
create
	init
feature {Any}
	unitType: UnitTypeCommonDescriptor
	identifier: String

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	
	init (ut: like unitType; id: like identifier; args: like arguments) is
	do
		identifier := id
		unitType := ut
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
	end -- init
	
	out: String is
	do
		if unitType = Void then
			Result := ""
		else
			Result := "{" + unitType.out + "} "
		end -- if
		if identifier = Void then
			Result.append_string ("return")
		else
			Result.append_string (identifier)
		end -- if
		Result.append_string (outArguments)
	end -- out
end -- class NewStatementDescriptor

class NewExpressionDescriptor
--  [new] UnitType [ Arguments ]
inherit
	ExpressionDescriptor
	end
	RoutineArguments
		undefine
			is_equal
		redefine
			out
	end
create
	init
feature {Any}
	unitType: NamedTypeDescriptor -- UnitTypeCommonDescriptor
	init (ut: like unitType; args: like arguments) is
	require
		non_void_unitType : ut /= Void
	do
		unitType := ut
		if args = Void then
			create arguments.make (1, 0)
		else
			arguments := args
		end -- if
	end -- init
	out: String is
	do
		Result := "new "
		Result.append_string (unitType.out)
		Result.append_string (outArguments)
	end -- out
	sameAs (other: like Current): Boolean is
	do
		Result := unitType.is_equal (other.unitType) and then sameArguments (other)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := unitType < other.unitType
		if not Result and then unitType.is_equal (other.unitType) then
			Result := lessArguments (other)
		end -- if
	end -- theSame
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		useConst: Sorted_Array [UnitTypeNameDescriptor]
		stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
	do
	useConst := context.useConst
	stringPool := context.stringPool
	typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	non_void_unitType : unitType /= Void
	non_void_arguments : arguments /= Void
end -- class NewExpressionDescriptor

-- ExpressionList: Expression{"," Expression}

class IfStatementDescriptor
-- IfCase:
-- if Expression (case Alternatives)|(BlockStart StatementsList)
-- {elsif Expression (case Alternatives)|(BlockStart StatementsList)}
-- [else [StatementsList]]
-- BlockEnd
inherit
	StatementDescriptor
	end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}
	ifParts: Array [IfLineDecsriptor]
	elsePart: Array [StatementDescriptor]
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		from
			i := ifParts.lower
			n := ifParts.upper
		until
			i > n
		loop
			if ifParts.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
		if elsePart /= Void then
			from
				i := elsePart.lower
				n := elsePart.upper
			until
				i > n
			loop
				if elsePart.item (i).isInvalid (context, o) then
					Result := True
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	init (eic: like ifParts; ep: like elsePart) is
	require
		consistent_if: eic /= Void and then eic.count > 0
	do
		ifParts := eic
		elsePart := ep
	end -- out

	out: String is
	local
		i, n: Integer
	do
		Result := "if " + ifParts.item (1).out + "%N"
		from
			i := 2
			n := ifParts.count
		until
			i > n
		loop
			Result.append_string (getIdent + "elsif " + ifParts.item (i).out)
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			i := i + 1
		end -- loop
		if elsePart /= Void then
			Result.append_string (getIdent + "else%N")
			incIdent
			from
				i := 1
				n := elsePart.count
			until
				i > n
			loop
				-- 
				Result.append_string (getIdent + elsePart.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character ('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if
		if Result.item (Result.count) /= '%N' then
			Result.append_character ('%N')
		end -- if
		Result.append_string(getIdent + "end // if%N")
	end -- out		
invariant
	consistent_if: ifParts /= Void and then ifParts.count > 0
end -- class IfStatementDescriptor

deferred class IfLineDecsriptor
-- if Expression (case Alternatives)|(BlockStart StatementsList)
-- or
-- {elsif Expression (case Alternatives)|(BlockStart StatementsList)}

inherit
	Comparable
		undefine
			out
		redefine
			is_equal
	end
	BuildServer
		undefine 
			is_equal, out
	end
feature {Any}
	expr: ExpressionDescriptor
	is_equal (other: like Current): Boolean is
	do
		Result := expr.is_equal (other.expr)
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		Result := expr < other.expr
	end -- infix "<"
invariant
	if_expression_not_void: expr /= Void
end -- class IfLineDecsriptor

class IfIsLineDecsriptor
-- if Expression case Alternatives
-- or
-- elsif Expression case Alternatives
inherit
	IfLineDecsriptor
		redefine
			is_equal, infix "<"
	end
	Identation
		undefine
			is_equal
	end
create
	init 
feature {Any}

--	alternatives: Array [IfStatementAlternative]
	alternatives: Array [AlternativeDescriptor]

	init (e: like expr; a: like alternatives) is
	require
		expression_non_void: e /= Void
		alternatives_not_void: a /= Void
	do
		expr := e
		--if a = Void then
		--	create alternatives.make (1, 0)
		--else	
			alternatives := a
		--end -- if
	end -- init

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	out: String is
	local
		i, n: Integer
	do
		Result := expr.out + "%N"
		incIdent
		from
			i := 1
			n := alternatives.count
		until
			i > n
		loop
			Result.append_string (getIdent + "case " + alternatives.item (i).out)
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			i := i + 1
		end -- loop
		decIdent
	end -- out

	is_equal (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := Precursor (other)
		if Result then
			n := alternatives.count
			Result := n = other.alternatives.count 
			if Result then
				from
					i := 1					
				until
					i > n
				loop
					if alternatives.item (i).is_equal (other.alternatives.item (i)) then
						i := i + 1
					else
						i := n + 1
						Result := False
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := Precursor (other)
		if Result then
			n := alternatives.count
			m := other.alternatives.count 
			Result := n < m
			if not Result and then n = m then
				from
					i := 1					
				until
					i > n
				loop
					if alternatives.item (i) < other.alternatives.item (i) then
						i := n + 1
						Result := True
					elseif alternatives.item (i).is_equal (other.alternatives.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- infix "<"
invariant
	alternatives_not_void: alternatives /= Void
end -- class IfIsLineDecsriptor

class IfDoLineDecsriptor
-- if Expression BlockStart StatementsList
-- or
-- elsif Expression BlockStart StatementsList
inherit
	IfLineDecsriptor
		redefine
			is_equal, infix "<"
	end
	Identation
		undefine
			is_equal
	end
create
	init 
feature {Any}
	statements: Array [StatementDescriptor]
	init (e: like expr; s: like statements) is
	require
		expression_non_void: e /= Void
	do
		expr := e
		if s = Void then
			create statements.make (1, 0)
		else
			statements := s
		end -- if
	end -- init

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- do nothing so far
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate


	out: String is
	local
		i, n: Integer
	do
		Result := expr.out + " do%N"
		incIdent
		from
			i := 1
			n := statements.count
		until
			i > n
		loop
			Result.append_string (getIdent + statements.item (i).out)
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			i := i + 1
		end -- loop
		decIdent
	end -- out
	is_equal (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := Precursor (other)
		if Result then
			n := statements.count
			Result := n = other.statements.count
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if statements.item (i).is_equal(other.statements.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		Result := Precursor (other)
		if not Result and then expr.is_equal (other.expr) then
			n := statements.count
			m := other.statements.count
			Result := n < m
			if not Result and then n = m then
				from
					i := 1
				until
					i > n
				loop
					if statements.item (i) < other.statements.item (i) then
						Result := True
						i := n + 1
					elseif statements.item (i).is_equal(other.statements.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- infix "<"
invariant
	statements_not_void: statements /= Void
end -- class IfDoLineDecsriptor

deferred class AlternativeDescriptor
-- AlternativeTags StatementsList
-- or
-- AlternativeTags Expression
inherit
	SmartComparable
		undefine
			out
	end
	BuildServer
		undefine
			out, is_equal
	end
feature {Any}
	alternativeTags: Sorted_Array[AlternativeTagDescriptor]
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		n := alternativeTags.count
		Result := n = other.alternativeTags.count
		if Result then
			from
				i := 1
			until
				i > n
			loop
				if alternativeTags.item (i).is_equal (other.alternativeTags.item (i)) then
					i := i + 1
				else
					Result := False
					i := n + 1
				end -- if
			end -- loop
		end -- if			
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := alternativeTags.count
		m := other.alternativeTags.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if alternativeTags.item (i) < other.alternativeTags.item (i) then
					Result := True
					i := n + 1
				elseif alternativeTags.item (i).is_equal (other.alternativeTags.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if			
	end -- lessThan
feature {AlternativeDescriptor}
	out_alternatives: String is
	local
		i, n: Integer
	do
		Result := ""
		from
			i := 1
			n := alternativeTags.count
		until
			i > n
		loop
			if i > 1 then
				Result.append_string (", ")
			end -- if
			Result.append_string (alternativeTags.item (i).out)
			i := i + 1
		end -- loop
		Result.append_character (' ')
		--Result.append_string (" do")
	end -- out_alternatives
	alternativeTagsValid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		i, n: Integer
		notValid: Boolean		
	do
		from
			i := 1
			n := alternativeTags.count
		until
			i > n
		loop
			notValid := alternativeTags.item (i).isInvalid (context, o)
			if notValid then
				Result := True
			end -- if
			i := i + 1
		end -- loop		
	end -- alternativeTagsValid

invariant
	alternativeTags_not_void: alternativeTags /= Void
end -- class AlternativeDescriptor

class IfStatementAlternative
-- ValueAlternative {"," ValueAlternative}  StatementsList  -- "do"
-- AlternativeTags StatementsList
inherit
	AlternativeDescriptor
	end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}

	statements: Array [StatementDescriptor]	
	
	init (a: like alternativeTags; s: like statements) is
	require
		alternativeTag_not_void: a /= Void 
	do
		alternativeTags := a
		if s = Void then
			create statements.make (1, 0)
		else
			statements := s
		end -- if
	end -- init

	out: String is
	local
		i, n: Integer
	do		
		Result := out_alternatives
		Result.append_character ('%N')
		incIdent
		from
			i := 1
			n := statements.count
		until
			i > n
		loop
			Result.append_string (getIdent + statements.item (i).out)
			if Result.item (Result.count) /= '%N' then
				Result.append_character ('%N')
			end -- if
			i := i + 1
		end -- loop
		decIdent
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		from
			i := 1
			n := statements.count
		until
			i > n
		loop
			if statements.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
	end -- is_invalid

	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

invariant
	statements_not_void: statements /= Void
end -- class IfStatementAlternative

class IfExpressionAlternative
-- "case" ValueAlternative {"," ValueAlternative} "do" Expression
-- AlternativeTags Expression
inherit
	AlternativeDescriptor
	end
create
	init
feature {Any}

	expr: ExpressionDescriptor
	
	init (a: like alternativeTags; e: like expr) is
	require
		alternativeTag_not_void: a /= Void 
		expression_not_void: e /= Void
	do
		alternativeTags := a
		expr := e
	end -- init

	out: String is
	do
		Result := out_alternatives
		Result.append_character (' ')
		Result.append_string (expr.out)
		Result.append_character (' ')
	end -- out
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		-- i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if expr.isInvalid (context, o) then
			Result := True
		end -- if
		if alternativeTagsValid (context, o) then
			Result := True
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
invariant
	expression_not_void: expr /= Void
end -- class IfExpressionAlternative

-- MemberDescription : ( [rtn] RoutineName [Signature] )|( Idenitifer ì:îUnitType )
deferred class MemberDescriptionDescriptor
inherit	
--	AlternativeDescriptor -- WHY IT INHERITS IT ?????
	SmartComparable
		undefine	
			out
--		redefine
--			is_equal, infix "<"
	end 
	BuildServer
		undefine
			out, is_equal
	end
	NamedDescriptor
		undefine
			is_equal
	end
feature
	--name: String
	getExternalName: String is
	do
		Result := clone (name)
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
invariant
	--not_void_name: name /= Void
end -- class MemberDescriptionDescriptor
class RoutineDescriptionDescriptor
inherit	
	MemberDescriptionDescriptor
	end 
create
	init
feature {Any}
	signature: SignatureDescriptor
	init (n: like name; s: like signature) is
	require
		not_void_name: n /= Void
		not_void_signature: s /= Void
	do
		name := n
		signature := s
	end -- init
	out: String is
	do
		Result := name + " " + signature.out
	end -- out
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- checkValidity

feature {MemberDescriptionDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name) and then signature.is_equal (other.signature)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			Result := signature < other.signature
		end -- if
	end -- lessThan
invariant
	not_void_signature: signature /= Void
end -- class RoutineDescriptionDescriptor

class AttributeDescriptionDescriptor
inherit	
	MemberDescriptionDescriptor
	end 
create
	init
feature {Any}
	type: UnitTypeCommonDescriptor
	init (n: like name; t: like type) is
	require
		not_void_name: n /= Void
		not_void_type: t /= Void
	do
		name := n
		type := t
	end -- init
	out: String is
	do
		Result := name + ": " + type.out
	end -- out
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- checkValidity

feature {MemberDescriptionDescriptor}
	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name) and then type.is_equal (other.type)
	end -- sameAs
	lessThan(other: like Current): Boolean is
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			Result := type < other.type
		end -- if
	end -- lessThan
invariant
	not_void_type: type /= Void
end -- class AttributeDescriptionDescriptor

class LoopStatementDescriptor
-- [while BooleanExpression] [RequireBlock] InnerBlockDescriptor [while BooleanExpression] [EnsureBlock] end
inherit	
	InnerBlockDescriptor
		rename
			init as InnerBlockInit
		export {None} InnerBlockInit
		redefine
			out
	end
create
	init
feature {Any}
	isWhileLoop: Boolean
	whileExpr: ExpressionDescriptor
	requireClause: Array[PredicateDescriptor]
	ensureClause: Array[PredicateDescriptor]
	setAssertions (preconditions, postconditions: Array[PredicateDescriptor]) is
	do
		if preconditions = Void then
			create requireClause.make (1, 0)
		else
			requireClause := preconditions
		end -- if
		if postconditions = Void then
			create ensureClause.make (1, 0)
		else
			ensureClause := postconditions
		end -- if
	end -- if
	out: String is
	local	
		i, n: Integer
	do
		--incIdent
		if isWhileLoop then
			Result := "while " + whileExpr.out + "%N"
		else	
			Result := ""
		end -- if
		
		n := requireClause.count
		if n > 0 then
			from
				Result.append_string (getIdent + "require%N")
				incIdent
				i := 1
			until
				i > n
			loop
				Result.append_string (getident + requireClause.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if

		Result.append_string (Precursor)
		if Result.item (Result.count) /= '%N' then
			Result.append_character ('%N')
		end -- if

		if not isWhileLoop then
			Result.append_string (getIdent + "while " + whileExpr.out)
		end -- if

		n := ensureClause.count
		if n > 0 then
			from
				Result.append_string (getIdent + "ensure%N")
				incIdent
				i := 1
			until
				i > n
			loop
				Result.append_string (getIdent + ensureClause.item (i).out)
				if Result.item (Result.count) /= '%N' then
					Result.append_character('%N')
				end -- if
				i := i + 1
			end -- loop
			decIdent
		end -- if
		if Result.item (Result.count) /= '%N' then
			Result.append_character ('%N')
		end -- if
		Result.append_string (getIdent + "end // loop%N")
		--decIdent
	end
	init (ioff: like invariantOffList; isWL: Boolean; w: like whileExpr; rc: like requireClause; stmts: like statements; wc: like whenClauses; wec: like whenElseClause; ec: like ensureClause) is
	require
		non_void_while_expression: w /= Void
		non_void_loop_body: stmts /= Void
	do
		InnerBlockInit (ioff, stmts, wc, wec)
		isWhileLoop := isWL
		whileExpr:= w
		if rc = Void then
			create requireClause.make (1, 0)
		else
			requireClause := rc
		end -- if
		if ec = Void then
			create ensureClause.make (1, 0)
		else
			ensureClause := ec
		end -- if
	end
invariant
	non_void_while_expression: whileExpr /= Void
	non_void_requireClause: requireClause /= Void
	non_void_ensureClause: ensureClause /= Void
end -- class LoopStatementDescriptor

-------------------- Types -------------

deferred class TypeOrExpressionDescriptor
inherit
	SmartComparable
		undefine
			out
	end
	BuildServer
		undefine
			out, is_equal
	end
feature
	getExternalName: String is
	deferred
	ensure
		non_void_external_name: Result /= Void
	end -- getExternalName
	getFactualGenericExternalName (pos: Integer): String is
	require
		valid_pos: pos > 0
	deferred
	ensure
		non_void_external_name: Result /= Void
	end -- getFactualGenericExternalName
end -- class TypeOrExpressionDescriptor

deferred class TypeDescriptor
-- Type: ["?"] AttachedTypeDescriptor
--  AttachedTypeDescriptor: UnitType|AnchorType|MultiType|TupleType|RangeType|RoutineType

-- UnitTypeDescriptor|AnchorTypeDescriptor|MultiTypeDescriptor|DetachableTypeDescriptor |TupleType|RangeTypeDescriptor|RoutineTypeDescriptor
-- identifier, as, ?, rtn, (
inherit	
	TypeOrExpressionDescriptor
	end
feature {Any}
	getFactualGenericExternalName (pos: Integer): String is
	do
		Result := pos.out
	end -- getFactualGenericExternalName
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	require
		non_void_context: context /= Void
	deferred
	end -- isNotLoaded
	
	aliasName: String is do end
	
end -- class TypeDescriptor

class AliasedTypeDescriptor
inherit
	TypeDescriptor
		redefine
			aliasName
	end
create
	init, make
feature {Any}
	aliasName: String
	actualType: AttachedTypeDescriptor
	init (aName: like aliasName; aType: like actualType) is
	require
		non_void_alias_name: aName /= Void
		non_void_real_type: aType /= Void
	do
		aliasName := aName
		actualType := aType
	end -- init
	make (aName: like aliasName) is
	require
		non_void_alias_name: aName /= Void
	do
		aliasName := aName
	end -- make
	out: String is
	do
		Result := aliasName + " alias for " + actualType.out
	end -- out
	sameAs (other: like Current) : Boolean is
	do
		Result := aliasName.is_equal (other.aliasName)
	end -- sameAs
	lessThan (other: like Current) : Boolean is
	do
		Result := aliasName < other.aliasName
	end -- lessThan

	getExternalName: String is
	do
		Result := actualType.getExternalName
	end -- getExternalName
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if actualType.isInvalid (context, o) then
			Result := True
		end -- if
	end -- is_invalid
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		if actualType.isNotLoaded (context, o) then
			Result := True
		end -- if
	end -- isNotLoaded
	
invariant
	non_void_alias_name: aliasName /= Void
	-- non_void_actual_type: actualType /= Void
end -- class AliasedTypeDescriptor

deferred class AttachedTypeDescriptor
-- AttachedType: UnitType|AnchorType|MultiType|TupleType|RangeType|RoutineType|AnonymousUnitType
inherit
	TypeDescriptor
		redefine
			aliasName
	end
feature {Any}
	aliasName: String
	setAliasName (aName: like aliasName) is
	do
		aliasName := aName
	end -- setAliasName
end -- class AttachedTypeDescriptor

class AnonymousUnitTypeDescriptor
-- AnonymousUnitType "type" MemberDesciption {[";"] MemberDesciption} "end"
-- MemberDescription: ([rtn] RoutineName[Signature])|(Idenitifer{","Idenitifer} ":" UnitType)
inherit	
	AttachedTypeDescriptor
	end
	Identation
		undefine
			is_equal
	end
create	
	init
feature {Any}	
	members: Sorted_Array [MemberDescriptionDescriptor]
	init (m: like members) is 
	do
		if m = Void then
			create members.make
		else
			members := m
		end -- if
	end -- init
	
	getExternalName: String is
	local
		i, n: Integer
	do
		Result := "type"
		from
			i := 1
			n := members.count
		until
			i > n
		loop
			Result.append_character('_')
			Result.append_string (members.item (i).getExternalName)
			i := i + 1
		end -- loop
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	out: String is 
	local
		i, n: Integer
	do
		Result := getIdent + "type "
		from
			i := 1
			n := members.count
			incIdent
			Result.append_string(getIdent)
		until
			i > n
		loop
			Result.append_string (members.item (i).out)
			if i \\ 4 = 0 then
				Result.append_character ('%N')
				Result.append_string (getIdent)
			else
				Result.append_character (' ')
			end -- if
			i := i + 1
		end -- loop
		decIdent
		Result.append_string (getIdent + "end")
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
	end -- isNotLoaded
	
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		from
			i := 1
			n := members.count
			Result := True
		until
			i > n
		loop
			if members.item (i).is_equal (other.members.item (i)) then
				i := i + 1
			else
				Result := False
				i := n + 1
			end -- if
		end -- loop
	end -- sameAs
	lessThan(other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := members.count
		m := other.members.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if members.item (i) < other.members.item (i) then
					Result := True
					i := n + 1
				elseif members.item (i).is_equal (other.members.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- lessThan
invariant
	non_void_members: members /= Void
end -- class AnonymousUnitTypeDescriptor

class RoutineTypeDescriptor 
-- rtn [SignatureDescriptor]
inherit	
	AttachedTypeDescriptor
	end
create	
	init
feature {Any}	
	signature: SignatureDescriptor 
	init (s: like signature) is 
	require
		non_void_signature: s /= Void
	do
		signature := s
	end -- init
	out: String is 
	do
		Result := "rtn " + signature.out
		--if aliasName /= Void then
		--	Result.append_string (" alias ")
		--	Result.append_string (aliasName)
		--end -- if
	end -- out
	getExternalName: String is
	do
		Result := "rtn " + signature.getExternalName
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		if signature.isInvalid (context, o) then
			Result := True
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	do
		-- There is nothing to load for the routine type - really ?		
	end -- isNotLoaded
	
	sameAs (other: like Current): Boolean is
	do
		Result := signature.is_equal (other.signature)
	end
	lessThan(other: like Current): Boolean is
	do
		Result := signature < other.signature
	end
invariant
	non_void_signature: signature /= Void
end -- class RoutineTypeDescriptor

class SignatureDescriptor
--41 "("[TypeDescriptor {"," TypeDescriptor}]")"[":" TypeDescriptor]
-- ("("[Type {"," Type}]")"[":" Type])| (":" Type)
inherit
	Comparable
		redefine
			out, is_equal
	end
	BuildServer
		redefine
			out, is_equal
	end
create	
	init
feature {Any}
	parameters: Array [TypeDescriptor]
	returnType: TypeDescriptor

	init (p: like parameters; t: like returnType) is
	do
		if p = Void then
			create parameters.make (1, 0)
		else
			parameters := p
		end -- if
		returnType := t
	end -- init

	out: String is
	local	
		i, n: Integer
	do
		n := parameters.count
		if n > 0 then
			from
				Result := " ("
				i := 1
			until	
				i > n
			loop
				Result.append_string (parameters.item (i).out)
				if i < n then
					Result.append_string (", ")
				end -- if
				i := i + 1
			end -- loop
			Result.append_character (')')
		else
			Result := ""
		end -- if
		if returnType /= Void then
			Result.append_string (": ")
			Result.append_string (returnType.out)
		end -- if
	end -- out

	getExternalName: String is
	local	
		i, n: Integer
	do
		n := parameters.count
		if n > 0 then
			from
				Result := "$"
				i := 1
			until	
				i > n
			loop
				Result.append_string (parameters.item (i).getExternalName)
				if i < n then
					Result.append_character ('_')
				end -- if
				i := i + 1
			end -- loop
			Result.append_character ('$')
		else
			Result := ""
		end -- if
		if returnType /= Void then
			Result.append_string ("$$")
			Result.append_string (returnType.getExternalName)
		end -- if
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	is_equal (other: like Current): Boolean is
	local	
		i, n: Integer
	do
		if returnType /= Void and then other.returnType /= Void then
			Result := returnType.is_equal (other.returnType)
		else
			Result := returnType = Void and then other.returnType = Void
		end -- if
		if Result then
			n := parameters.count
			Result := n = other.parameters.count
			if Result then
				from
					i := 1
				until
					i > n
				loop
					if parameters.item (i).is_equal (other.parameters.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- is_equal
	infix "<"(other: like Current): Boolean is
	local	
		i, n, m: Integer
		checkParameters: Boolean
	do
		if returnType = Void then
			if other.returnType = Void then
				checkParameters := True
			else
				Result := True
			end -- if
		elseif other.returnType /= Void then
			if returnType < other.returnType then
				Result := True
			elseif returnType.is_equal (other.returnType) then
				checkParameters := True
			end -- if
		end -- if		

		--if returnType /= Void and then other.returnType /= Void then
		--	Result := returnType < other.returnType
		--else
		--	Result := returnType = Void and then other.returnType /= Void
		--end -- if
		
		if checkParameters then
			n := parameters.count
			m := other.parameters.count
			if n = m then
				from
					i := 1
				until
					i > n
				loop
					if parameters.item (i) < other.parameters.item (i) then
						Result := True
						i := n + 1
					elseif parameters.item (i).is_equal (other.parameters.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			else
				Result := n < m
			end -- if
		end -- if
	end -- infix <
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		from
			-- All parameters are to be valid
			n := parameters.count
			i := 1
		until	
			i > n
		loop
			if parameters.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
		if returnType /= Void then
			-- Return type is to be be valid
			if returnType.isInvalid (context, o) then
				Result := True
			end -- if
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	non_void_parameters: parameters /= Void
end -- class SignatureDescriptor

-- ConstantExpression : Expression

deferred class ConstExpressionDescriptor
inherit
	ExpressionDescriptor
	end
feature
end -- class ConstExpressionDescriptor

deferred class RangeTypeDescriptor
-- RangeType: 
-- 	(ConstantExpression ["{"OperatorName ConstantExpression"}"] ".." ConstantExpression)
-- 	|
-- 	(ConstantExpression {"|" ConstantExpression})

inherit
	AttachedTypeDescriptor
	end
end -- class RangeTypeDescriptor

class FixedRangeTypeDescriptor
-- 	(ConstantExpression ["{"OperatorName ConstantExpression"}"] ".." ConstantExpression)
inherit
	RangeTypeDescriptor
	end
create
	init
feature {Any}
	--left, right: ConstExpressionDescriptor
	left, right: ExpressionDescriptor
	operator: String
	--expr: ConstExpressionDescriptor -- ExpressionDescriptor
	expr: ExpressionDescriptor
	init (l: like left; o: like operator; e: like expr; r: like right) is
	require
		non_void_left: l /= Void
		non_void_right: r /= Void
		consistent_reg_expr: o /= Void implies e /= Void
	do
		left := l
		operator:= o
		expr:= e
		right:= r
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := left.is_equal (other.left) and then right.is_equal (other.right)
-- Not checked !!! Not_implemented_yet
--	operator: String
--	expr: ExpressionDescriptor

	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := left < other.left
		if not Result and then left.is_equal (other.left) then 
			Result := right < (other.right)
		end -- if
	end -- lessThan
	out: String is
	do
		Result := left.out
		if operator /= Void then
			Result.append_string (" {")
			Result.append_string (operator)
			Result.append_character (' ')
			Result.append_string (expr.out)
			Result.append_character ('}')			
		end -- if
		Result.append_string (" .. ")
		Result.append_string (right.out)
		--if aliasName /= Void then
		--	Result.append_string (" alias ")
		--	Result.append_string (aliasName)
		--end -- if
	end -- out
	getExternalName: String is
	do
		Result := left.out
		if operator /= Void then
			Result.append_character ('$')
			Result.append_string (operator)
			Result.append_character ('_')
			Result.append_string (expr.out)
			Result.append_character ('$')
		end -- if
		Result.append_string ("$$")
		Result.append_string (right.out)
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
	end -- isNotLoaded
	
invariant
	non_void_left: left /= Void
	non_void_right: right /= Void
	consistent_reg_expr: operator /= Void implies expr /= Void
end -- class FixedRangeTypeDescriptor

class EnumeratedRangeTypeDescriptor
-- 	(ConstantExpression {"," ConstantExpression})
inherit
	RangeTypeDescriptor
	end
create
	init
feature {Any}
	--values: Array [ConstExpressionDescriptor]
	values: Array [ExpressionDescriptor]
	init (v: like values) is
	require
		values_not_void: v /= Void
	do
		values := v
	end -- init

	getExternalName: String is
	local
		i, n: Integer
	do
		from
			i := 1
			n := values.count
			Result := ""
		until
			i > n
		loop
			Result.append_string (values.item (i).out)
			if i < n then
				Result.append_character ('_')
			end -- if
			i := i + 1
		end -- loop
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	out: String is
	local
		i, n: Integer
	do
		from
			i := 1
			n := values.count
			Result := ""
		until
			i > n
		loop
			Result.append_string (values.item (i).out)
			if i < n then
				Result.append_string (", ")
			end -- if
			i := i + 1
		end -- loop
	end -- if

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		-- Values should be of the same common ancestor type ... ?
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		--i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
	end -- isNotLoaded

	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		from
			i := 1
			n := values.count
			Result := True
		until
			i > n
		loop
			if values.item (i).is_equal (other.values.item (i)) then
				i := i + 1
			else
				Result := False
				i := n + 1
			end -- if
		end -- loop
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := values.count
		m := other.values.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if values.item (i) < other.values.item (i) then
					Result := True
					i := n + 1
				elseif values.item (i).is_equal (other.values.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- lessThan
invariant
	values_not_void: values /= Void
end -- class EnumeratedRangeTypeDescriptor

class AsThisTypeDescriptor
inherit
	AnchoredCommonDescriptor
	end
feature {Any}
	out: String is do Result := "as this" end
	sameAs (other: like Current): Boolean is
	do
		Result := True
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
	end -- lessThan
	getExternalName: String is
	do
		Result := "as_this"
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	is_invalid, isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		--notValid: Boolean		
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
	end -- isInvalid, isNotLoaded
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

end -- class AsThisTypeDescriptor

deferred class AnchoredCommonDescriptor
inherit
	AttachedTypeDescriptor
	end
feature {Any}
	autoGen: Boolean
	setAutoGen (b: Boolean) is
	do
		autoGen := b
	end -- setAutoGen
end -- class AnchoredCommonDescriptor

class AnchorTypeDescriptor
-- as Identifier [Signature]
inherit
	AnchoredCommonDescriptor
	end
create
	init
feature {Any}
	anchorId: String
	anchorSignature: SignatureDescriptor
	init (a: like anchorId; s: like anchorSignature) is
	require
		anchor_not_void: a /= Void
	do
		anchorId := a
		anchorSignature := s
	end -- init
	out: String is
	do
		Result := "as " + anchorId
		if anchorSignature /= Void then
			Result.append_string (anchorSignature.out)
		end -- if
	end -- out
	getExternalName: String is
	do
		Result := "as_" + anchorId
		if anchorSignature /= Void then
			Result.append_string (anchorSignature.getExternalName)
		end -- if
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if anchorSignature /= Void then
			if anchorSignature.isInvalid (context, o) then
				Result := True
			end -- if
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	do	
		-- do nothing so far. Or we need to have signature types loaded ...
	end -- isNotLoaded

	sameAs (other: like Current): Boolean is
	do
		Result := anchorId.is_equal (other.anchorID)
		if Result and then anchorSignature /= Void then
			if other.anchorSignature = Void then
				Result := False
			else
				Result := anchorSignature.is_equal (other.anchorSignature)
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := anchorId < other.anchorID
		if not Result and then anchorId.is_equal (other.anchorID) then
			Result := anchorSignature = Void and then other.anchorSignature /= Void
			if not Result and then anchorSignature /= Void and then other.anchorSignature /= Void then
				Result := anchorSignature < other.anchorSignature
			end -- if
		end -- if
	end -- lessThan
invariant
	anchor_not_void: anchorId /= Void
end -- class AnchorTypeDescriptor

class MultiTypeDescriptor
-- UnitTypeDescriptor {"|" UnitTypeDescriptor} 
inherit
	AttachedTypeDescriptor
	end
create
	init
feature {Any}
	types: Array [UnitTypeCommonDescriptor]
	init (t: like types) is
	require
		non_void_types: t /= Void
		valid_multi_type: t /= Void implies t.count > 1
	do
		types := t
	end -- init
	getExternalName: String is
	local
		i, n: Integer
	do
		from
			Result := ""
			i := 1
			n := types.count
		until
			i > n
		loop
			Result.append_string(types.item(i).getExternalName)
			if i < n then
				Result.append_character('$')
			end -- if
			i := i + 1
		end -- loop
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	out: String is
	local
		i, n: Integer
	do
		from
			Result := ""
			i := 1
			n := types.count
		until
			i > n
		loop
			Result.append_string(types.item(i).out)
			if i < n then
				Result.append_string(" | ")
			end -- if
			i := i + 1
		end -- loop
		--if aliasName /= Void then
		--	Result.append_string (" alias ")
		--	Result.append_string (aliasName)
		--end -- if
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		from
			i := 1
			n := types.count
		until
			i > n
		loop
			if types.item(i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		from
			i := 1
			n := types.count
		until
			i > n
		loop
			if types.item(i).isNotLoaded (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
	end -- isNotLoaded
	
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		n := types.count
		Result := n = other.types.count 
		if Result then
			from
				i := 1
			until
				i > n
			loop
				if types.item (i).is_equal (other.types.item (i)) then
					i := i + 1
				else	
					i := n + 1
					Result := False
				end -- if	
			end -- loop
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := types.count
		m := other.types.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if types.item (i) < other.types.item (i) then
					Result := True
					i := n + 1
				elseif types.item (i).is_equal (other.types.item (i)) then
					i := i + 1
				else	
					i := n + 1
				end -- if	
			end -- loop
		end -- if
	end -- lessThan
invariant
	non_void_types: types /= Void
	valid_multi_type: types /= Void implies types.count > 1
end -- class MultiTypeDescriptor

class DetachableTypeDescriptor
-- "?" AttachedTypeDescriptor
inherit
	TypeDescriptor
	end
create
	init
feature {Any}
	type: AttachedTypeDescriptor
	init (t: like type) is
	require
		non_void_type: t /= Void
	do
		type := t
	end -- init
	out: String is
	do
		result := "?" + type.out
	end -- out
	getExternalName: String is
	do
		Result := "$" + type.getExternalName
	end -- getExternalName
	
	sameAs (other: like Current): Boolean is
	do
		Result := type.is_equal (other.type)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := type < other.type
	end -- lessThan
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if type.isInvalid (context, o) then
			Result := True
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
		-- Check
		-- type should be attached !!!
		-- not_implemened_yet
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	do
		if type.isNotLoaded (context, o) then
			Result := True
		end -- if
	end -- isNotLoaded

invariant
	non_void_type: type /= Void
end -- class DetachableTypeDescriptor

class TupleTypeDescriptor
-- "("[TupleFieldDescriptor {","|";" TupleFieldDescriptor}]")"
inherit
	AttachedTypeDescriptor
	end
create
	init --, fill
feature {Any}
	fields: Array [TupleFieldDescriptor]
	--fill (f: like fields) is
	--do
	--	if f = Void then
	--		create fields.make (1, 0)
	--	else
	--		fields := f
	--	end -- if
	--end -- fill
	init is
	do
		create fields.make (1, 0)
	end -- init
	sameAs (other: like Current): Boolean is
	local
		i, n : Integer
	do
		n := fields.count
		Result := n = other.fields.count 
		if Result then
			from
				i := 1
			until
				i > n
			loop
				if fields.item (i).is_equal (other.fields.item (i)) then
					i := i + 1
				else
					i := n + 1
					Result := False
				end -- if
			end -- loop
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m : Integer
	do
		n := fields.count
		m := other.fields.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if fields.item (i) < other.fields.item (i) then
					Result := True
					i := n + 1
				elseif fields.item (i).is_equal (other.fields.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- loop
		end -- if
	end -- lessThan
	out: String is
	local
		i, n : Integer
	do
		from
			Result := "("
			i := 1
			n := fields.count
		until
			i > n
		loop
			Result.append_string (fields.item (i).out)
			if i < n then
				Result.append_string(", ")
			end -- if
			i := i + 1
		end -- loop
		Result.append_character (')')
		--if aliasName /= Void then
		--	Result.append_string (" alias ")
		--	Result.append_string (aliasName)
		--end -- if
	end -- out
	getExternalName: String is
	local
		i, n : Integer
	do
		from
			Result := "$"
			i := 1
			n := fields.count
		until
			i > n
		loop
			Result.append_string (fields.item (i).getExternalName)
			if i < n then
				Result.append_character('_')
			end -- if
			i := i + 1
		end -- loop
		Result.append_character ('$')
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		from
			i := 1
			n := fields.count
		until
			i > n
		loop
			if fields.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop		
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]
		--notValid: Boolean
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- not_implemened_yet
		from
			i := 1
			n := fields.count
		until
			i > n
		loop
			if fields.item (i).isNotLoaded (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop		
	end -- isNotLoaded
	
invariant
	non_void_fields: fields /= Void
end -- class TupleTypeDescriptor

deferred class TupleFieldDescriptor
inherit
	SmartComparable
		undefine
			out
	end
	BuildServer
		undefine
			out, is_equal
	end
feature
	getExternalName: String is
	deferred
	ensure
		non_void_external_name: Result /= Void
	end -- getExternalName
	type: NamedTypeDescriptor -- UnitTypeCommonDescriptor
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	do
		if type.isNotLoaded (context, o) then
			Result := True
		end -- if
	end -- isNotLoaded
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	do
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		if type.IsInvalid (context, o) then
			Result := True
		end -- if
	end -- checkValidity
invariant
	non_void_type: type /= Void
end -- class TupleFieldDescriptor

class TypedTupleFieldDescriptor
inherit
	TupleFieldDescriptor
	end
create
	init
feature {Any}
	out: String is
	do
		Result := type.out
	end -- out
	getExternalName: String is
	do
		Result := type.getExternalName
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	sameAs (other: like Current): Boolean is
	do
		Result := type.is_equal (other.type) 
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := type < other.type
	end -- lessThan

	init (t: like type) is
	require
		non_void_type: t /= Void	
	do
		type := t
	end -- init
end -- class TypedTupleFieldDescriptor

class NamedTupleFieldDescriptor
inherit
	TupleFieldDescriptor
	end
	NamedDescriptor
		undefine
			is_equal
	end
create
	init
feature {Any}
	--name: String
	out: String is
	do
		Result := "" + name
		Result.append_string (": ")
		Result.append_string (type.out)
	end -- out
	getExternalName: String is
	do
		Result := "" + name
		Result.append_character ('$')
		Result.append_string (type.getExternalName)
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName

	sameAs (other: like Current): Boolean is
	do
		Result := name.is_equal (other.name) and then type.is_equal (other.type) 
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			Result := type < other.type
		end -- if
	end -- lessThan
	init (n: like name; t: like type) is
	require
		non_void_type: t /= Void	
		non_void_name: n /= Void
	do
		type := t
		name := n
	end -- init
invariant
	--non_void_name: name /= Void
end -- class NamedTupleFieldDescriptor

class UnitTypeDescriptor
--  [ref|val|concurrent] UnitTypeNameDescriptor
inherit
	UnitTypeCommonDescriptor
		redefine
			out
	end
create
	init
feature {Any}
	isRef,
	isVal,
	isConcurrent: Boolean
	out: String is
	do
		if isRef then
			Result := "ref "
		elseif isVal then
			Result := "val "
		elseif isConcurrent then
			Result := "concurrent "
		else	
			Result := ""
		end	-- if
		Result.append_string (Precursor)
	end -- out
	
	sameAs (other: like Current): Boolean is
	do
		Result := isRef = other.isRef and then isVal = other.isVal and then isConcurrent = other.isConcurrent and then sameNameAndGenerics (other)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := isRef and then not other.isRef
		if not Result and then isRef = other.isRef then
			Result := isVal and then not other.isVal
			if not Result and then isVal = other.isVal then
				Result := isConcurrent and then not other.isConcurrent
				if not Result and then isConcurrent = other.isConcurrent then
					Result := lessNameAndGenerics (other)
				end -- if
			end -- if
		end -- if		
	end -- lessThan

	init (ir, iv, ic: Boolean; aName: like name; g: like generics) is
	require
		general_consistence: ir or else iv or else ic
		is_ref_consistent: ir implies not iv and then not ic 
		is_val_consistent: iv implies not ir and then not ic 
		is_concurrent_consistent: ic implies not iv and then not ir
	do
		isRef := ir
		isVal := iv
		isConcurrent := ic
		setNameAndGenerics (aName, g)
	end -- init

invariant
	is_ref_consistent: isRef implies not isVal and then not isConcurrent 
	is_val_consistent: isVal implies not isref and then not isConcurrent 
	is_concurrent_consistent: isConcurrent implies not isVal and then not isRef
	general_consistence: isRef or else isVal or else isConcurrent
end -- class UnitTypeDescriptor

deferred class NamedTypeDescriptor
inherit
	AttachedTypeDescriptor
	end
	ExpressionDescriptor
		undefine
			is_equal, infix "<", getExternalName, getFactualGenericExternalName
	end
	NamedDescriptor
		undefine
			is_equal
	end
feature {Any}
	--name: String
end -- class NamedTypeDescriptor

deferred class UnitTypeCommonDescriptor
inherit
	NamedTypeDescriptor
	end
	--TupleFieldDescriptor
	--end
feature {Any}
--	name: String
	generics: Array [TypeOrExpressionDescriptor]
	setNameAndGenerics (aName: like name; g: like generics) is
	require	
		non_void_unit_type_name: aName /= Void
	do
		name := aName
		if g = Void then
			create generics.make (1, 0)
		else
			generics := g
		end -- if
	end -- setNameAndGenerics
	out: String is
	local
		i, n: Integer
	do
		Result := "" + name
		n := generics.count
		if n > 0 then
			from
				Result.append_string (" [")
				i := 1
			until
				i > n
			loop
				Result.append_string (generics.item (i).out)
				if i < n then
					Result.append_string (", ")
				end -- if
				i := i + 1
			end -- loop
			Result.append_character (']')
		end -- if
		--if aliasName /= Void then
		--	Result.append_string (" alias ")
		--	Result.append_string (aliasName)
		--end -- if
	end -- out
	getExternalName: String is
	local
		i,n : Integer
	do
		Result := "" + name
		n := generics.count
		if n > 0 then
			from
				Result.append_character ('$')
				i := 1
			until
				i > n
			loop
				Result.append_character ('_')
				Result.append_string (generics.item (i).getFactualGenericExternalName (i))
				--Result.append_string (i.out)
				i := i + 1
			end -- loop
			Result.append_character ('$')
		end -- if
		Result.append_string ("_" + buildHash (Result))
	end -- getExternalName
	
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		typePool: Sorted_Array[TypeDescriptor]	
		-- notValid: Boolean
		pos: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		typePool := context.typePool
		-- do nothing so far
		pos := typePool.seek (Current)
		check
			unit_registered : pos > 0
		end -- check
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	isNotLoaded (context: CompilationUnitCommon; o: Output): Boolean is
	local
		toRegister: Boolean
		typePool: Sorted_Array[TypeDescriptor]
		unitTypeDsc: UnitTypeCommonDescriptor
	do
		if interface = Void then
			if generics.count > 0 then
				typePool := context.typePool
-- Need to search for the generic type not the instantiation !!! Replace Current with the proper node!!!
				unitTypeDsc ?= typePool.search (Current)
				if unitTypeDsc = Void then
debug
--	o.putNL ("Unit type `" + out + "` is NOT found in the type pool")
end -- debug
				else
debug
--	o.putNL ("Unit type `" + out + "` is found in the type pool")
end -- debug
					interface := unitTypeDsc.interface
				end -- if
				if interface = Void then
					toRegister := True
					-- ask system desciption to load type interface 
					interface := context.loadUnitInterface (getExternalName, out, o, Current)
					if interface /= Void then
						interface.instantiate (generics)
					end -- if
				end -- if
			else
				-- ask system desciption to load type interface 
				interface := context.loadUnitInterface (getExternalName, out, o, Current)
			end -- if
			if interface = Void then
				Result := True
			elseif toRegister then				
				-- Regsiter the 'interface' in the pool
				typePool.add (interface.type.getUnitTypeDescriptor) -- type: UnitDeclarationDescriptor
			end -- if
			if not Result and then aliasName /= Void then
				-- Register alias if set in the pool
				-- Not_implemented_yet !!!
				-- type A alias B end  type C alias B end  to be detected !!!
			end -- if
		end -- if
	end -- isNotLoaded
	interface: CompilationUnitUnit
	
	sameNameAndGenerics (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := name.is_equal (other.name)
		if Result then
			n := generics.count
			if n = other.generics.count then
				from
					i := 1
				until
					i > n
				loop
					if generics.item (i).is_equal (other.generics.item (i)) then
						i := i + 1
					else
						Result := False
						i := n + 1
					end -- if
				end -- loop
			else
				Result := False
			end
		end -- if
	end -- sameNameAndGenerics
	
	lessNameAndGenerics (other: like Current): Boolean is
	local
		i, n: Integer
	do
		Result := name < other.name
		if not Result and then name.is_equal (other.name) then
			n := generics.count
			Result := n < other.generics.count
			if not Result and then n = other.generics.count then
				from
					i := 1
				until
					i > n
				loop
					if generics.item (i) < other.generics.item (i) then
						Result := True
						i := n + 1
					elseif generics.item (i).is_equal(other.generics.item (i)) then
						i := i + 1
					else
						i := n + 1
					end -- if
				end -- loop
			end -- if
		end -- if
	end -- lessNameAndGenerics
invariant
	non_void_unit_type_name: name /= Void
	non_void_generics: generics /= Void
end -- class UnitTypeCommonDescriptor

class UnitTypeNameDescriptor
-- Identifier ["["Type {"," Type}"]"]
inherit
	UnitTypeCommonDescriptor
		rename
			setNameAndGenerics as init,
			sameNameAndGenerics as sameAs,
			lessNameAndGenerics as lessThan
	end
--	ExpressionDescriptor
--		undefine
--			is_equal, infix "<", getExternalName, getFactualGenericExternalName
--	end
	--EntityDescriptor
	--	undefine
	--		is_equal, infix "<", getExternalName, getFactualGenericExternalName
	--end 
create
	init

end -- class UnitTypeNameDescriptor


-----------------------------------------------------------------
class IfExpressionDescriptor
-- if Expression (case ExpressionAlternatives)|( BlockStart Expression)
-- {elsif Expression (case ExpressionAlternatives)|( BlockStart Expression)}
-- else Expression ‚Äú}‚ÄùCmod
--
--ExpressionAlternatives: ‚Äúcase‚Äù AlternativeTags "do" Expression {‚Äúcase‚Äù AlternativeTags "do" Expression}
inherit
	ExpressionDescriptor
	end	
create 
	init
feature {Any}
	ifExprLines: Array [IfLineDecsriptor]
	--ifExprLines: Array [IfExprLineDescriptor]
	elseExpr: ExpressionDescriptor

	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		from
			i := 1
			n := ifExprLines.count
		until
			i > n
		loop
			if ifExprLines.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
		if elseExpr /= Void then
			if elseExpr.isInvalid (context, o) then
				Result := True
			end -- if
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate

	init (iif: like ifExprLines; e: like elseExpr) is
	require
		consistent_if: iif /= Void and then iif.count > 0
	do
		ifExprLines := iif
		elseExpr := e
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := "if " + ifExprLines.item (1).out
		from
			i := 2
			n := ifExprLines.count
		until
			i > n
		loop
			Result.append_string (" elsif " + ifExprLines.item (i).out)
			i := i + 1
		end -- if
		if elseExpr /= Void then
			Result.append_string (" else " + elseExpr.out)
		end -- if
	end -- init
	sameAs (other: like Current): Boolean is
	local
		i, n: Integer
	do
		n := ifExprLines.count
		Result := n = other.ifExprLines.count 
		if Result then
			from
				i := 1
			until
				i > n
			loop
				if ifExprLines.item (i).is_equal (other.ifExprLines.item (i)) then
					i := i + 1
				else
					Result := False
					i := n + 1
				end -- if
			end -- if
			if Result then
				if elseExpr = Void then
					Result := other.elseExpr = Void
				elseif other.elseExpr /= Void then
					Result := elseExpr.is_equal (other.elseExpr)
				end -- if
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	local
		i, n, m: Integer
	do
		n := ifExprLines.count
		m := other.ifExprLines.count
		Result := n < m
		if not Result and then n = m then
			from
				i := 1
			until
				i > n
			loop
				if ifExprLines.item (i) < other.ifExprLines.item (i) then
					Result := True
					i := n + 1
				elseif ifExprLines.item (i).is_equal (other.ifExprLines.item (i)) then
					i := i + 1
				else
					i := n + 1
				end -- if
			end -- if
			if Result then
				if elseExpr = Void then
					Result := other.elseExpr /= Void
				elseif other.elseExpr /= Void then
					Result := elseExpr < other.elseExpr
				end -- if
			end -- if
		end -- if
	end -- lessThan
invariant
	consistent_if: ifExprLines /= Void and then ifExprLines.count > 0
end -- class IfExpressionDescriptor

class IfIsExprLineDescriptor
-- (if | elsif Expression) case IfBodyExpression
inherit
	--IfExprLineDescriptor
	IfLineDecsriptor
	end
	Identation
		undefine
			is_equal
	end
create
	init
feature {Any}
	alternatives: Array [IfExpressionAlternative]
	
	init (e: like expr; a: like alternatives) is
	require
		non_void_expr: e /= Void
		non_void_body: a /= Void
	do
		expr:= e
		alternatives:= a
	end -- init
	out: String is
	local
		i, n: Integer
	do
		Result := expr.out + "%N"
		incIdent
		from
			i := 1
			n := alternatives.count
		until
			i > n
		loop
			Result.append_string (getIdent + "case " + alternatives.item (i).out)
			if i < n then
				Result.append_character(' ')
			end -- if
			i := i + 1
		end -- loop
		decIdent
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
		i, n: Integer
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if expr.isInvalid (context, o) then
			Result := True
		end -- if
		from
			i := 1
			n := alternatives.count
		until
			i > n
		loop
			if alternatives.item (i).isInvalid (context, o) then
				Result := True
			end -- if
			i := i + 1
		end -- loop
		if not Result then
			-- expr and all alternatives are valid then check types accordance
			-- not_implemented_yet
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
invariant
	consistent_is_body: alternatives /= Void
end -- class IfIsExprLineDescriptor

class IfDoExprLineDescriptor
-- (if | elsif Expression) do Expression
inherit
	--IfExprLineDescriptor
	IfLineDecsriptor
	end
create
	init
feature {Any}
	doExpr: ExpressionDescriptor
	init (e: like expr; de: like doExpr) is
	require
		do_expr_not_void: de /= Void
	do
		expr:= e
		doExpr:= de
	end -- init
	out: String is
	do
		Result := expr.out + " do " + doExpr.out
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		-- 'expr' is valid and of Boolean type
		if expr.isInvalid (context, o) then
			Result := True
		end -- if
		-- 'doExpr' is valid
		if doExpr.isInvalid (context, o) then
			Result := True
		end -- if
		if not Result then
debug
	o.putLine ("Validity check for: " + out)
end -- debug
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
	
invariant
	do_expr_not_void: doExpr /= Void
end -- class IfDoExprLineDescriptor
	
deferred class AlternativeTagDescriptor
-- Expression [[GroupStart OperatorName ConstantExpression GroupEnd] ‚Äú..‚ÄùExpression ] | UnitType
inherit
	SmartComparable
		undefine
			out
	end
	BuildServer
		undefine
			is_equal, out
	end
feature {Any}
end -- class AlternativeTagDescriptor

class UnitTypeAlternative
 -- UnitType
 inherit
 	AlternativeTagDescriptor
 	end
	ExpressionDescriptor
	end
 create
 	init
 feature {Any}
 	unitTypeDsc: NamedTypeDescriptor -- UnitTypeCommonDescriptor
 	init (t: like unitTypeDsc) is
 	require
 		unit_type_not_void: t /= Void 
 	do
 		unitTypeDsc := t
 	end -- init
 	sameAs (other: like Current): Boolean is
 	do
 		Result := unitTypeDsc.sameAs (other.unitTypeDsc)
 	end -- sameAs
 	lessThan (other: like Current): Boolean is
 	do
 		Result := unitTypeDsc < other.unitTypeDsc
 	end -- lessThan
 	out: String is
 	do
 		Result := unitTypeDsc.out
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if unitTypeDsc.isInvalid (context, o) then
			Result := True
		else
debug
	o.putLine ("Validity check for: " + out)
end -- debug
			-- expr should be either Type or value ....
			-- not_implemented_yet
		end -- if
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate	
invariant
	unit_type_not_void: unitTypeDsc /= Void 
end -- class UnitTypeAlternative

class ExpressionAlternative
-- Expression
inherit
	AlternativeTagDescriptor
	end
create
	init
feature {Any}
	expression: ExpressionDescriptor
	init (e: like expression) is
	require
		non_void_expression: e /= Void
	do
		expression := e
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := expression.is_equal (other.expression)
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := expression < other.expression
	end -- lessThan
	out: String is
	do
		Result := expression.out
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if expression.isInvalid (context, o) then
			Result := True
		else
debug
	o.putLine ("Validity check for: " + out)
end -- debug
			-- expr should be either Type or value ....
			-- not_implemented_yet
		end -- if
	end -- is_invalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
invariant
	non_void_expression: expression /= Void
end -- class ExpressionAlternative

class RangeAlternative
-- Expression ["|"OperatorName ConstantExpression] ".." Expression
inherit
	ExpressionAlternative
		rename 
			expression as lower,
			init as non_used_init
		export {None} non_used_init
		redefine	
			out, sameAs, lessThan, is_invalid, generate
	end
create
	init
feature {Any}
	operator: String
	constExpr: ExpressionDescriptor
	upper: ExpressionDescriptor
	init (l: like lower; o: like operator; ce: like constExpr; u: like upper) is
	require
		non_void_lower: l /= Void
		non_void_upper: u /= Void
		consistent_reg_exp: o /= Void implies ce /= Void
	do
		lower:= l
		operator:= o
		constExpr:= ce
		upper:= u
	end -- init
	sameAs (other: like Current): Boolean is
	do
		Result := lower.is_equal (other.lower) and then upper.is_equal (other.upper)
		if Result then
			Result := operator = Void and then other.operator = Void
			if not Result and then operator /= Void and then other.operator /= Void then
				Result := operator.is_equal (other.operator) and then constExpr.is_equal (other.constExpr)
			end -- if
		end -- if
	end -- sameAs
	lessThan (other: like Current): Boolean is
	do
		Result := lower < other.lower
		if not Result and then lower.is_equal (other.lower) then
			Result := upper < other.upper			
			if not Result and then upper.is_equal (other.upper) then
				Result := operator < other.operator
				if not Result and then operator.is_equal (other.operator) then
					Result := constExpr < other.constExpr
				end -- if
			end -- if
		end -- if
	end -- lessThan
	out: String is
	do
		Result := ""
		Result.append_string (lower.out)
		if operator /= Void then
			Result.append_string ("| ")
			Result.append_string (operator)
			Result.append_character (' ')
			Result.append_string (constExpr.out)
		end -- if
		Result.append_string (" .. ")
		Result.append_string (upper.out)
	end -- out
	is_invalid (context: CompilationUnitCommon; o: Output): Boolean is
	local
		--useConst: Sorted_Array [UnitTypeNameDescriptor]
		--stringPool: Sorted_Array [String]
		--typePool: Sorted_Array[TypeDescriptor]	
	do
	--useConst := context.useConst
	--stringPool := context.stringPool
	--typePool := context.typePool
		if lower.isInvalid (context, o) then
			Result := True
		end -- if
		if upper.isInvalid (context, o) then
			Result := True
		end -- if
		if operator /= Void then
			if constExpr.isInvalid (context, o) then
				Result := True
			else
				-- applicatiopn of operator with constExpr is to be valid
			end -- if
		end -- if
debug
	o.putLine ("Validity check for: " + out)
end -- debug
			-- expr should be either Type or value ....
			-- not_implemented_yet
	end -- isInvalid
	generate (cg: CodeGenerator) is
	do
		-- do nothing so far
	end -- generate
invariant
	non_void_upper: upper /= Void
	consistent_reg_exp: operator /= Void implies constExpr /= Void
end -- class RangeAlternative
-----------------------------------------------------------------
