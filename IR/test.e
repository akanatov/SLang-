-- new 			create
-- abstract		deferred
-- {			do
-- }			end
-- base			inherit
-- constructor	creation
-- =			:=
-- ==			=

class Test
inherit
	Displayable
	end
creation
	init
feature
	init is
	do
		create source.make (1, 0)
		currentFileName:= ""
		currentLine:= Void
		currentLinePos:= 0
		display (1)
	end
	source: Array [LineDsc]
	currentLine: LineDsc
	currentLinePos: Integer
	currentFileName: String
	
	save (fileName: String) is	
	local
		storage: AccordStorage
		file: File
		was_error: Boolean
	do
		if was_error then
			if fileName /= Void then
				sys.message_error ("Failed to save ...", "No way to save your program in file '" + fileName + "'")
			else
				sys.message_error ("Failed to save ...", "No way to save your program when file name not provied")
			end 
		elseif fileName /= Void and then fileName.count > 0 then
			create file.make_create_read_write (fileName)
			create storage.init (source) 
			storage.independent_store (file)
			file.close
			currentFileName := fileName
		end 
	rescue
		was_error := True
		retry
	end

	load (fileName: String) is
	local
		storage: AccordStorage
		file: File
		was_error: Boolean
	do
		if was_error then
			if fileName /= Void then
				sys.message_error ("Failed to load ...", "No way to load your program from the file '" + fileName + "'")
			else
				sys.message_error ("Failed to load ...", "No way to load your program due to file name not provied")
			end 
		elseif fileName /= Void and then fileName.count > 0 then
			create file.make_open_read (fileName)
			create storage.init (<<>>)
			storage ?= storage.retrieved (file)
			file.close
			if storage /= Void then
				source := storage.source
				currentLine:= Void
				currentLinePos:= 0
				currentFileName:= fileName
			end			
		end 
	rescue
		was_error := True
		retry
	end
	
	canAddEntry: Boolean is
	local
		i, n: Integer
		endCount: Integer
	do
		if source.count = 0 then
			Result := True
		else
			from
				i := 1
				n := source.count
			until
				i > n
			loop
				if i = currentLinePos then
					Result := endCount = 0
					i := n + 1
				else
					if source.item (i) = anEnd then
						endCount := endCount - 1
					else
						endCount := endCount + source.item(i).endCount
					end -- if
					i := i + 1
				end -- if
			end -- loop
		end -- if
	end -- canAddEntry
	
	addEntry is
	local
		entryDsc: EntryDsc
	do
		create entryDsc.init ("")
		entryDsc.display (1)
		if entryDsc.data_was_accepted then
			if hasEntry (entryDsc) then
				sys.message_error ("Duplicated entry ...", "No way to add one more entry with the same name '" + entryDsc.name + "'")
			elseif source.count = 0 or else currentLinePos >= source.count or else currentLinePos = 0 then
				source.force (entryDsc, source.count + 1)
				source.force (anEnd, source.count + 1)
				currentLinePos := source.count - 1
				currentLine:= entryDsc
			else
				source.force (entryDsc, currentLinePos)
				source.force (anEnd, currentLinePos + 1)
				currentLine:= entryDsc
			end
		end 
	end -- addEntry
	
	hasEntry (anEntry: EntryDsc): Boolean is
	require
		entry_not_void: anEntry /= Void
	local
		i, n: Integer
		entryDsc: EntryDsc
	do
		from
			i := 1
			n := source.count
		until
			i > n
		loop
			entryDsc ?= source.item (i)
			if entryDsc /= Void and then entryDsc.name.is_equal (anEntry.name) then
				Result := True
				i := n + 1
			else
				i := i + 1
			end -- if
		end -- loop
	end -- hasEntry
	
	addVar is
	local	
		varDsc: VarDsc
	do
		create varDsc.init ("", "")
		varDsc.display (1)
		if varDsc.data_was_accepted then
			if source.count = 0 or else currentLinePos >= source.count  or else currentLinePos = 0 then
				source.force (varDsc, source.count + 1)
				currentLinePos := source.count
			else
				source.force (varDsc, currentLinePos)
			end -- if
			currentLine:= varDsc
		end -- if
	end -- addVar

	canAddStatement: Boolean is
	local
		i, n: Integer
		endCount: Integer
	do
		from
			i := 1
			n := source.count
		until
			i > n
		loop
			if i = currentLinePos then
				Result := endCount > 0
				i := source.count + 1
			else
				if source.item (i) = anEnd then
					endCount := endCount - 1
				else
					endCount := endCount + source.item(i).endCount
				end -- if
				i := i + 1
			end -- if
		end -- loop
	end -- canAddStatement
	
	addAssign is
	local	
		assignDsc: AssignDsc
	do
		create assignDsc.init ("", "")
		assignDsc.display (1)
		if assignDsc.data_was_accepted then
			if source.count = 0 or else currentLinePos >= source.count  or else currentLinePos = 0 then
				source.force (assignDsc, source.count + 1)
				currentLinePos := source.count
			else
				source.force (assignDsc, currentLinePos)
			end
			currentLine:= assignDsc
		end 
	end -- addAssign

	addIf is
	local	
		ifDsc: IfDsc
	do
		create ifDsc.init ("")
		ifDsc.display (1)
		if ifDsc.data_was_accepted then
			if source.count = 0 or else currentLinePos >= source.count  or else currentLinePos = 0 then
				source.force (ifDsc, source.count + 1)
				source.force (anEnd, source.count + 1)
				source.force (anElse, source.count + 1)
				source.force (anEnd, source.count + 1)
				currentLinePos := source.count - 3
			else
--				resize ()
--				insert ()
				
				source.force (ifDsc, currentLinePos)
				source.force (anEnd, currentLinePos + 1)
				source.force (anElse, currentLinePos + 2)
				source.force (anEnd, currentLinePos + 3)
			end
			currentLine:= ifDsc
		end 
	end -- addIf

	currentLineInPlace: Boolean is
	do
		Result := currentLine /= Void and then currentLinePos > 0 and then currentLinePos <= source.count
	end
	canDeleteCurrentBlock: Boolean is
	do
		Result := currentLineInPlace and then currentLine /= anEnd
	end -- canDeleteCurrentBlock
	deleteCurrentLineBlock is
	require
		can_delete:	canDeleteCurrentBlock
	local
		endCount: Integer
		numberOfRemoved: Integer
	do
		endCount := currentLine.endCount
		source.remove (currentLinePos)
		numberOfRemoved := numberOfRemoved + 1
		if endCount > 0 then
			from
			until
				endCount = 0
			loop
				if source.item (currentLinePos ) = anEnd then
					endCount := endCount - 1
				end -- if
				source.remove (currentLinePos)
				numberOfRemoved := numberOfRemoved + 1
			end -- loop
		end -- if
		source.resize (1, source.count - numberOfRemoved)
		if source.count = 0 then
			currentLinePos := 0
			currentLine := Void
		end -- if
	end
	anEnd: EndDsc is
	once
		create Result
	end
	anElse: ElseDsc is
	once
		create Result
	end
invariant
	non_void_source: source /= Void
end

class AccordStorage
-- Service to support persistency of the IR
inherit
	Storable
	end
creation
	init
feature
	source: Array [LineDsc]
	init (src: like source) is
	require
		source_not_void: src /= Void
	do
		source := src
	end
end

-- Start of IR classes
deferred class LineDsc
inherit
	Displayable
		undefine
			out
	end
feature
	endCount: Integer is
	deferred
	end -- endCount
end -- class LineDsc

class EntryDsc
inherit 
	LineDsc
	end
creation
	init
feature
	name: String
	endCount: Integer is 1

	out: String is
	do
		Result := "entry"
		if name.count > 0 then
			Result.append_character (' ')
			Result.append_string (name)
		end 
		Result.append_string (" {")
	end -- out
	init (aName: like name) is
	require
		non_void_name: aName /= Void
	do
		name := aName
	end -- init
invariant
	name_not_void: name /= Void
end -- class EntryDsc

class EndDsc
inherit 
	LineDsc
	end
feature
	out: String is "}"
	endCount: Integer is 0
end -- class EndDsc

class VarDsc
inherit 
	LineDsc
	end
creation
	init
feature
	name: String
	type: String
	endCount: Integer is 0
	out: String is
	do
		Result := "var "
		Result.append_string (name)
		Result.append_string (": ")
		Result.append_string (type)
	end 
	init (aName: like name; aType: like type) is
	require
		non_void_name: aName /= Void
		non_void_type: aType /= Void
	do
		name := aName
		type := aType
	end
invariant
	non_void_name: name /= Void
	non_void_type: type /= Void
end -- class VarDsc

class IfDsc
inherit 
	LineDsc
	end
creation
	init
feature
	expression: String
	endCount: Integer is 2 -- if expr {} else {}  => 2 }
	out: String is
	do
		Result := "if "
		Result.append_string (expression)
		Result.append_string (" {")
	end 
	init (anExpr: like expression) is
	require
		non_void_expression: anExpr /= Void
	do
		expression := anExpr
	end
invariant
	non_void_expression: expression /= Void
end -- class IfDsc

class ElseDsc
inherit 
	LineDsc
	end
feature
	endCount: Integer is 0
	out: String is "else {"
end -- class ElseDsc

class AssignDsc
inherit 
	LineDsc
	end
creation
	init
feature
	target: String
	expression: String
	endCount: Integer is 0
	out: String is
	do
		Result := ""
		Result.append_string (target)
		Result.append_string (" := ")
		Result.append_string (expression)
	end 
	init (aTarget: like target; anExpr: like expression) is
	require
		non_void_target: aTarget /= Void
		non_void_expression: anExpr /= Void
	do
		target := aTarget
		expression := anExpr
	end
invariant
	non_void_expression: expression /= Void
	non_void_target: target /= Void
end -- class AssignDsc

