class FileSystem
inherit
	File_System
		rename
			cluster_exists as folderExists,
			add_cluster as createFolder			
	end
feature {Any}
	younger (path1, path2: String): Boolean is
		-- path1 < path2 in terms of date 
	require
		path1_not_void: path1 /= Void
		path2_not_void: path2 /= Void
	local
		wasError: Boolean
	do
		if not wasError then
--print ("%N%TPath1: " + path1 + ", time: " + file_time (path1).out + "%N")
--print ("%N%TPath2: " + path2 + ", time: " + file_time (path2).out + "%N")
--io.read_character
			Result := file_time (path1) < file_time (path2)
		end -- if
	rescue
		if not wasError then	
			wasError := True
			retry
		end -- if
	end -- younger
	getAllFilesWithExtension (path, ext: String): Array [Fsys_Dat] is
	require
		path_not_void: path /= Void
		extension_not_void: ext /= Void
	local
		allFiles: Array [Fsys_Dat]
		fsysDat: Fsys_Dat
		fileExt: String
		i, n, m: Integer
		
	do
		allFiles := file_list (path)
		if allFiles /= Void then
			n := allFiles.count
			create Result.make (1, n)
			from
				i := 1
			until
				i > n
			loop
				fsysDat := allFiles.item (i)
				fileExt := getFileExtension(fsysDat.name)
				if fileExt.is_equal (ext) then
					m := m + 1
					Result.put (fsysDat, m)
				end -- if
				i := i + 1
			end -- loop
			if m < n then
				Result.resize (1, m)
			end -- if
		else
			Result := <<>>
		end -- if
	end -- getAllFilesWithExtension
	folderCreated (folderName: String): Boolean is
	require
		folderName_not_void: folderName /= Void
	do
		if Result then
			Result := False
		else
			createFolder (folderName, "rwe")
			Result := True
		end
	rescue
		Result := True
		retry
	end -- folderCreated

	getFileExtension(name: String): String is
	require	
		file_name_not_void: name /= Void
	local 
		i: Integer
		found: Boolean
	do
		from
			i := name.count
			Result := ""
		until
			i = 0
		loop
			inspect
				name.item (i)
			when '\', '/', ':' then
				i := 0
			when '.' then
				i := 0
				found := True
			else
				Result.insert_character (name.item (i).as_lower, 1)
				i := i - 1
			end -- if
		end -- loop
		if not found then
			Result := ""
		end -- if
	ensure
		extension_not_void: Result /= Void
		valid_length: Result.count <= name.count
	end -- getFileExtension

	getFilePath (name: String): String is
	require	
		file_name_not_void: name /= Void
	local 
		i: Integer
	do
		from
			i := name.count
		until
			i = 0
		loop
			inspect
				name.item (i)
			when '\', '/', ':' then
				Result := name.substring(1, i)
				i := 0
			else	
				i := i - 1
			end
		end -- loop
		if Result = Void then
			Result := "."
		end -- if
	ensure
		file_path_not_void: Result /= Void
	end -- getFilePath
	
	getFileName(name: String): String is
	require	
		file_name_not_void: name /= Void
	local 
		pos: Integer
		start, stop: Integer
	do
		from
			pos := name.count
		until
			pos = 0
		loop
			inspect
				name.item (pos)
			when '\', '/', ':' then
				start := pos + 1
				pos := 0
			when '.' then
				pos := pos - 1
				if stop = 0 then
					stop := pos
				end
			else	
				pos := pos - 1
			end
		end -- loop
		if start = 0 then
			if stop > 0 then
				Result := name.substring (1, stop)
			else
				Result := name
			end -- if
		elseif stop = 0 then	
			Result := name.substring (start, name.count)
		elseif start <= stop then	
			Result := name.substring (start, stop)
		else
			Result := name
		end -- if
	ensure
		file_name_not_void: Result /= Void
	end -- getFileName
	
	remove_files_with_the_same_name (fileName: String) is
	require
		non_void_fileName: fileName /= Void
	local	
		files: Array [FSys_Dat]
		aName: String
		i, n: Integer
	do
		files := file_list (getFilePath (fileName))
		if files /= Void then
			from
				aName := getFileName (fileName)
				i := 1
				n := files.count
			until
				i > n
			loop
				if getFileName (files.item (i).name).is_equal (aName) then
					silent_remove_file(files.item (i).path)
				end -- if
				i := i + 1
			end -- loop
		end -- if
	end -- remove_files_with_the_same_name
	
	get_files_with_the_same_name (fileName: String): Array [FSys_Dat] is
	require
		non_void_fileName: fileName /= Void
	local	
		files: Array [FSys_Dat]
		aName: String
		i, n, m: Integer
	do
		files := file_list (getFilePath (fileName))
		if files = Void then
			create Result.make (1, 0)
		else
			from
				aName := getFileName (fileName)
				n := files.count
				create Result.make (1, n)
				i := 1
			until
				i > n
			loop
				if getFileName (files.item (i).name).is_equal (aName) then
					m := m + 1
					Result.put (files.item (i), m)
				end -- if
				i := i + 1
			end -- loop
			Result.resize (1, m)
		end -- if
	ensure	
		non_void_list: Result /= Void
	end -- get_files_with_the_same_name

	
	loadObjectFromFile (aFile: File): Storable is
	require
		file_not_void: aFile /= Void
		file_is_open_read: aFile.is_open_read
	do
		Result := stread (aFile.handle)
	ensure
		object_loaded : Result /= Void
	end -- loadObjectFromFile
	objectStoredToFile (object: Storable; aFile: File): Boolean is
	require
		file_not_void: aFile /= Void
		file_is_open_read: aFile.is_open_write
		object_not_void: object /= Void
	local
		wasError: Boolean
	do
		if not wasError then
			object.independent_store (aFile)
			Result := True
		end -- if
	rescue
		wasError := True
		retry
	end -- objectStoredToFile
	
feature {None}
	silent_remove_file(fileName: String) is
	require
		non_void_fileName: fileName /= Void
	local	
		wasError: Boolean
	do
		if not wasError then
			remove_file(fileName)
		end -- if
	rescue -- we just ignore the error
		wasError := True
		retry
	end -- silent_remove_file

   stread (handle: INTEGER): STORABLE is
      external "C"
         alias "ReadObj"
      end
end -- class FileSystem