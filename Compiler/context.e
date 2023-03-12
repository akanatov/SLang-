class ContextUnit
inherit
	Comparable
		redefine
			is_equal, out
	end
creation
	init
feature
	id: Integer
	unitDsc: ContextTypeDescriptor -- UnitDeclarationDescriptor
	parents: Sorted_Array [like Current]
	children:  Sorted_Array [like Current]

	isVirtual: Boolean is
	do
		Result := unitDsc.getUnitDeclaration.isVirtual
	end	-- isVirtual
	
	isTemplate: Boolean is
	do
		Result := unitDsc.isTemplate
	end -- isTemplate

	out: String is
	local
		index: Integer
		inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]
		unitMembers: Sorted_Array [MemberDeclarationDescriptor]	
	do
		Result := "%T#:" + id.out + " unit `" + unitDsc.fullUnitName + "`"
		index := parents.count
		if index > 0 then
			from
				Result.append_string (" has " + index.out + " parents:")
			until
				index = 0
			loop
				Result.append_character (' ')
				Result.append_string (parents.item(index).unitDsc.fullUnitName)
				index := index - 1
			end -- loop
			Result.append_character (';')
		end -- if
		index := children.count
		if index > 0 then
			from
				Result.append_string (" has " + index.out + " children: ")
			until
				index = 0
			loop
				Result.append_string (children.item(index).unitDsc.fullUnitName + "/" + children.item(index).children.count.out)
				index := index - 1
				if index /= 0 then
					Result.append_character (',')
				end -- if
			end -- loop	
			Result.append_character (';')
		end -- if
		inheritedOverrides := unitDsc.getUnitDeclaration.inheritedOverrides
		index := inheritedOverrides.count
		if index > 0 then
			from
				Result.append_string (" override ")
			until
				index = 0
			loop
				Result.append_string (inheritedOverrides.item(index).out)
				index := index - 1
				if index /= 0 then
					Result.append_character (',')
				end -- if
			end -- loop	
		end -- if
		unitMembers := unitDsc.getUnitDeclaration.unitMembers
		index := unitMembers.count
		if index > 0 then
			from
				Result.append_string (" members")
			until
				index = 0
			loop
				Result.append_character (' ')
				Result.append_string (unitMembers.item(index).fullMemberName)
				index := index - 1
			end -- loop	
		end -- if
	end -- out
	setSortByChildrenCount is
	do
		sortMode.setMode (childrenMode)
	end -- setSortByChildrenCount
	setSortByID is
	do
		sortMode.setMode (idMode)
	end -- setSortByID
	is_equal (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
		when defaultMode then
			Result := unitDsc.is_equal (other.unitDsc)
		when idMode then
			Result := id = other.id
		when childrenMode then
			Result := children.count = other.children.count and then id = other.id
		end -- inspect
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
		when defaultMode then
			Result := unitDsc < other.unitDsc
		when idMode then
			Result := id < other.id
		when childrenMode then
			Result := children.count < other.children.count
			if not Result and then children.count = other.children.count then
				Result := id < other.id
			end -- if
		end -- inspect
	end -- infix "<"
	setID(an_id: Integer) is
	require
		valid_id: an_id >= -1	
	do
		id := an_id
	end -- setID
	addParent (parentUnit:like Current) is
	require
		non_void_parent: parentUnit /= Void
	do
		parents.add (parentUnit)
		parentUnit.children.add (Current)
	end -- addParent
feature {None}
	sortMode: Mode is
	once
		create Result
	end -- sortMode
	init (unit:like unitDsc) is
	require
		non_void_unit: unit /= Void
	do
		unitDsc := unit
		create parents.make
		create children.make
		sortMode.setMode(defaultMode)
	end -- init
	defaultMode:  Character is 'D'
	idMode: Character is '#'
	childrenMode: Character is 'C'
invariant
	valid_id: id >= -1
	non_void_unit: unitDsc /= Void
	non_void_parents: parents /= Void
	non_void_children: children /= Void
end -- class ContextUnit
class Mode
feature {ContextUnit}
	mode: Character
	setMode (m: Character) is
	do
		mode := m
	end -- setMode
end -- class Mode

