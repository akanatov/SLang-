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
	contextTypeDsc: ContextTypeDescriptor -- UnitDeclarationDescriptor
	parents: Sorted_Array [ContextUnit]
	children: Sorted_Array [ContextUnit]
	members: Sorted_Array [MemberInVectorDescriptor]

	isVirtual: Boolean is
	do
		Result := contextTypeDsc.getUnitDeclaration.isVirtual
	end	-- isVirtual
	
	isTemplate: Boolean is
	do
		Result := contextTypeDsc.isTemplate
	end -- isTemplate
	
	isGeneric: Boolean is
	do
		Result := contextTypeDsc.isGeneric
	end -- isGeneric

	--buildRowVector  (heigth: Integer) is
	--		contextTypeDsc.buildRowVector (heigth)
	--buildOwnMemebrsList is
	--do
	--	create members.make
	--end -- buildOwnMemebrsList
	
	findSimilarMembers (parentMember: MemberInVectorDescriptor): Sorted_Array [MemberInVectorDescriptor] is
	local
		index: Integer
		member: MemberInVectorDescriptor
	do
		from
			index := members.count
		until
			index = 0
		loop
			member := members.item (index)
			if member.version.name.is_equal (parentMember.version.name) then
				if member.isOverriding then
					-- ???
				else
					-- ???
				end -- if
				if Result = Void then
					create Result.fill (<<member>>) 
				else
					Result.add (member)
				end -- if
			end -- if
			index := index - 1
		end -- loop
		if Result /= Void then
			-- ???? XXX
		end -- if
	end -- findSimilarMembers
	
	getOverridingMembers: Sorted_Array [MemberInVectorDescriptor] is
	local
		unitDclDsc: UnitDeclarationDescriptor
	do
		unitDclDsc := contextTypeDsc.getUnitDeclaration
		check
			unitDclDsc /= Void
		end -- check
	end -- getOverridingMembers
	
	buildFlatForm (o: Output) is
	local
		overridingMembers: Sorted_Array [MemberInVectorDescriptor]
		parent: ContextUnit
		pMembers: Sorted_Array [MemberInVectorDescriptor]
		oMembers: Sorted_Array [MemberInVectorDescriptor]
		parentMember: MemberInVectorDescriptor
		--overridingMember: MemberInVectorDescriptor
		inheritedMember: InheritedMemberInVectorDescriptor
		pIndex: Integer
		mIndex: Integer
		--pos: Integer
	do
		overridingMembers := getOverridingMembers
		from
			pIndex := parents.count
		until
			pIndex = 0
		loop
			parent := parents.item (pIndex)
			pMembers := parent.members
			from
				mIndex := pMembers.count
			until
				mIndex = 0
			loop
				parentMember := pMembers.item (mIndex)
				oMembers := findSimilarMembers (parentMember)
				if oMembers = Void then
					-- simply inherited !
					create inheritedMember.makeFromMember (parentMember)
					members.add (inheritedMember)
				else
					-- there are several members beign inherited under the same name + signature
				end -- oMembers
				--pos := members.seek (inheritedMember)
				--if pos <= 0 then
				--	members.add_after (inheritedMember, pos)
				--else
				--	-- overriding ?
				--	overridingMember := members.item (pos)
				--	if overridingMember.isOverriding then
				--		-- Not implemented yet !!! Check for conformance !!!
				--	else
				--		-- Duplicating declaration detected !!!
				--		o.putNL (
				--			"Duplicating member inherited in unit `" + contextTypeDsc.fullUnitName + "` member `" + overridingMember.version.fullMemberName +
				--			"` from parent `" + parent.contextTypeDsc.fullUnitName + "`"
				--		)
				--	end -- if					
				--end -- if
				mIndex := mIndex - 1
			end -- loop
			pIndex := pIndex - 1
		end -- loop
	end -- buildFlatForm
	
	out: String is
	local
		index: Integer
		inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]
		unitMembers: Sorted_Array [MemberDeclarationDescriptor]	
	do
		Result := "%T#" + id.out + " - `" + contextTypeDsc.fullUnitName + "`"
		if False then
			index := parents.count
			if index > 0 then
				from
					Result.append_string (" has " + index.out + " parents:")
				until
					index = 0
				loop
					Result.append_character (' ')
					Result.append_string (parents.item(index).contextTypeDsc.fullUnitName)
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
					Result.append_string (children.item(index).contextTypeDsc.fullUnitName + "/" + children.item(index).children.count.out)
					index := index - 1
					if index /= 0 then
						Result.append_character (',')
					end -- if
				end -- loop	
				Result.append_character (';')
			end -- if
		end -- if
		index := members.count
		if index > 0 then
			from
				Result.append_character (':')
			until
				index = 0
			loop
				Result.append_character ('%T')
				Result.append_string (members.item(index).out)
				index := index - 1
			end -- loop	
		else
			inheritedOverrides := contextTypeDsc.getUnitDeclaration.inheritedOverrides
			index := inheritedOverrides.count
			if index > 0 then
				from
					Result.append_string (" override: ")
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
			unitMembers := contextTypeDsc.getUnitDeclaration.unitMembers
			index := unitMembers.count
			if index > 0 then
				from
					Result.append_string (" members:")
				until
					index = 0
				loop
					--Result.append_character (' ')
					Result.append_string (unitMembers.item(index).fullMemberName)
					index := index - 1
					if index /= 0 then
						Result.append_character (',')
					end -- if
				end -- loop	
			end -- if
		end -- if
	end -- out
	
	sortMemebrsByID is
	do
		if members.count > 0 then
			members.item (1).setSortByID
			members.qsort
		end -- if
	end -- sortMemebrsByID
		
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
			Result := contextTypeDsc.is_equal (other.contextTypeDsc)
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
			Result := contextTypeDsc < other.contextTypeDsc
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
	sortMode: UnitSortMode is
	once
		create Result
	end -- sortMode
	init (unit: like contextTypeDsc) is
	require
		non_void_unit: unit /= Void
	local
		index: Integer
		mbrDsc: MemberInVectorDescriptor
		unitMembers: Sorted_Array [MemberDeclarationDescriptor]
		mbrDclDsc: MemberDeclarationDescriptor
	do
		contextTypeDsc := unit
		create parents.make
		create children.make
		sortMode.setMode(defaultMode)
		create members.make
		from
			unitMembers := contextTypeDsc.getUnitDeclaration.unitMembers
			index:= unitMembers.count
		until
			index = 0
		loop
			mbrDclDsc := unitMembers.item (index)
			if mbrDclDsc.isOverriding then
				-- How to find the seed !!!
				create mbrDsc.init (mbrDclDsc, contextTypeDsc, Void, Void)
			else
				create mbrDsc.init (mbrDclDsc, contextTypeDsc, mbrDclDsc, contextTypeDsc)
			end -- if
			members.add (mbrDsc)
			index := index - 1
		end -- loop
	end -- init
	defaultMode:  Character is 'D'
	idMode: Character is '#'
	childrenMode: Character is 'C'
invariant
	valid_id: id >= -1
	non_void_context_type: contextTypeDsc /= Void
	non_void_parents: parents /= Void
	non_void_children: children /= Void
end -- class ContextUnit
class UnitSortMode
feature {ContextUnit}
	mode: Character
	setMode (m: Character) is
	do
		mode := m
	end -- setMode
end -- class UnitSortMode

class InheritedMemberInVectorDescriptor
inherit
	MemberInVectorDescriptor
		redefine
			isOverriding
	end
creation
	makeFromMember
feature
	isOverriding: Boolean is False
feature {None}
	makeFromMember (other: MemberInVectorDescriptor) is
	require
		other_not_void: other /= Void
	do
		version := other.version
		versionUnit := other.versionUnit
		seed := other.seed
		origin := other.origin
		id := other.id
	end -- makeFromMember
end -- class InheritedMemberInVectorDescriptor

class MemberInVectorDescriptor
inherit
	Comparable
		redefine
			is_equal, out
	end
creation
	init
feature
	version: MemberDeclarationDescriptor
	versionUnit: ContextTypeDescriptor
	seed: MemberDeclarationDescriptor
	origin: ContextTypeDescriptor
	id: Integer
	
	isOverriding: Boolean is
	do
		Result := version.isOverriding
	end -- isOverriding
	
	is_equal (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
		when defaultMode then
			--Result := seed.is_equal (other.seed) and then version.is_equal (other.version)
			Result := version.is_equal (other.version)
			if Result then
				if seed /= other.seed and then seed /= Void and then other.seed /= Void then
					Result := seed.is_equal (other.seed) and then origin.is_equal (other.origin)
				end -- if
			end -- if
		when idMode then
			Result := id = other.id
		end -- inspect
	end -- is_equal
	infix "<" (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
		when defaultMode then
			Result := version < other.version
			if not Result and then version.is_equal (other.version) then
				Result := seed = Void and then other.seed /= Void
				if not Result and then seed /= Void and then other.seed /= Void then
					Result := seed < other.seed
					if not Result then
						Result := origin < other.origin
					end -- if
				end -- if
			end -- if
		when idMode then
			Result := id < other.id
		end -- inspect
	end -- infix "<"
	out: String is
	do
		Result := clone(version.fullMemberName)
		Result.append_character('@')
		Result.append_string(versionUnit.fullUnitName)
		--Result.append_character('-')
		--if version = seed then
		--	-- Start of the member version
		--	check
		--		seed /= Void
		--		origin /= Void
		--	end -- check
		--	Result.append_string(seed.fullMemberName)
		--	Result.append_character('$')
		--	Result.append_string(origin.fullUnitName)
		--else
		--	if seed = Void then
		--		Result.append_string("<Void>")
		--	else
		--		Result.append_string(seed.fullMemberName)
		--		Result.append_character('$')
		--		Result.append_string(origin.fullUnitName)
		--	end -- if
		--end -- if
	end -- out
	setSortByID is
	do
		sortMode.setMode (idMode)
	end -- setSortByID
	setSeedAndOrigin (aSeed: like seed; anOrigin: like origin) is
	require
		non_void_seed: aSeed /= Void	
		non_void_origin: anOrigin /= Void	
	do
		seed := aSeed
		origin := anOrigin
	end -- setSeed
feature {None}
	sortMode: MemberSortMode is
	once
		create Result
	end -- sortMode
	init (aVersion: like version; aVersionUnit: ContextTypeDescriptor; aSeed: like seed; anOrigin: like origin) is
	require
		non_void_version: aVersion /= Void
		non_void_version_unit: aVersionUnit /= Void
		--non_void_seed: aSeed /= Void
	do
		seed := aSeed
		version := aVersion
		versionUnit := aVersionUnit
		origin := anOrigin
		sortMode.setMode(defaultMode)
	end -- init
	defaultMode:  Character is 'D'
	idMode: Character is '#'
invariant
	non_void_version: version /= Void
	non_void_version_unit: versionUnit /= Void
	--non_void_seed: seed /= Void
end -- class MemberInVectorDescriptor
class MemberSortMode
feature {MemberInVectorDescriptor}
	mode: Character
	setMode (m: Character) is
	do
		mode := m
	end -- setMode
end -- class MemberSortMode
