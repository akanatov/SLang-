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
--print ("%N%TPath1: " + path1 + ", time: " + file_time (path1).out)
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
	getFileName(name: String): String is
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
				Result := name.substring(1, i - 1)
				i := 0
				found := True
			else	
				i := i - 1
			end
		end
		if not found then
			Result := name
		end -- if
	ensure
		extenstion_not_void: Result /= Void
	end -- getFileName
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
   stread (handle: INTEGER): STORABLE is
      external "C"
         alias "ReadObj"
      end
end -- class FileSystem