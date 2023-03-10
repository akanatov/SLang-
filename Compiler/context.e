class ContextUnit
inherit
	Comparable
		redefine
			is_equal
	end
feature
	id: Integer
	unitDsc: UnitDeclarationDescriptor
	parents: Sorted_Array [like Current]
	children:  Sorted_Array [like Current]
	is_equal (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
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
		valid_id: an_id >= 0	
	do
		id :=an_id
	end -- setID
	addParent (unit:like Current) is
	require
		non_void_parent: unit /= Void
	do
		parents.add (unit)
	end -- addParent
	addChild (unit:like Current) is
	require
		non_void_child: unit /= Void
	do
		sortMode.setMode(childrenMode)
		children.add (unit)
		sortMode.setMode(idMode)
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
		sortMode.setMode(idMode)
	end -- init
	idMode: Character is '#'
	childrenMode: Character is 'C'
invariant
	valid_id: id >= 0
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

