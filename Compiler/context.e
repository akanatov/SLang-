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
	flatFormBuilt: Boolean

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

	ConformingSignatures (currentVersion, inheritedVersion: MemberDeclarationDescriptor): Boolean is
	require
		currentVersion /= Void
		inheritedVersion /= Void
	do
		Result := currentVersion.conformsTo (inheritedVersion)
	end -- ConformingSignatures
	
	theSameSignatures (currentVersion, inheritedVersion: MemberDeclarationDescriptor): Boolean is
	require
		currentVersion /= Void
		inheritedVersion /= Void
	do
		Result := currentVersion.signatureIdenticalTo (inheritedVersion)
	end -- theSameSignatures

	sameSignatures (parentVersion: MemberDeclarationDescriptor; inheritedOverride: InheritedMemberOverridingDescriptor): Boolean is
	require
		parentVersion /= Void
		inheritedOverride /= Void
	do
		Result := parentVersion.hasTheSameSignature (inheritedOverride.signature)
	end -- sameSignatures
	
	failedToBuildFlatForm (o: Output): Boolean is
	local
		parent: ContextUnit
		inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]
		ioMembers: Sorted_Array [InheritedOverridingMemberInVectorDescriptor]
		imoDsc: InheritedMemberOverridingDescriptor
		pMembers: Sorted_Array [MemberInVectorDescriptor]
		parentMember: MemberInVectorDescriptor
		inheritedMember: InheritedMemberInVectorDescriptor
		inheritedOverridingMember: InheritedOverridingMemberInVectorDescriptor
		member: MemberInVectorDescriptor
		pIndex: Integer
		mIndex: Integer
		index: Integer
		ioCount: Integer
		toAddAsInherited: Boolean
		pos: Integer
	do
		if not flatFormBuilt then
			inheritedOverrides := contextTypeDsc.getUnitDeclaration.inheritedOverrides
			if inheritedOverrides /= Void then
				ioCount := inheritedOverrides.count
			end -- if
			from
				pIndex := parents.count
				debug
					o.putNL (">>>Building flatform for `" + contextTypeDsc.fullUnitName + "` Checking " + pIndex.out + " parents")
				end -- debug
			until
				pIndex = 0
			loop
				parent := parents.item (pIndex)
				debug
					o.putNL ("%T>Checking parent `" + parent.contextTypeDsc.fullUnitName + "`")
				end -- debug
				if not parent.flatFormBuilt then
					if parent.failedToBuildFlatForm (o) then
						Result := True
					end -- if
				end -- if
				
				if ioCount > 0 then -- There are overrides while inheriting
					pMembers := clone (parent.members) -- Copy of the flat form of the parent
					create ioMembers.make
					from
						--inheritedOverridingMember := Void
						index := ioCount
					until
						index = 0
					loop
						imoDsc := inheritedOverrides.item (index)
							--parent: UnitTypeNameDescriptor
							--name: String
							--signature: SignatureDescriptor
						if parent.contextTypeDsc.getUnitDeclaration.matchesType(imoDsc.parent) then
							-- Overriding member version from this parent
							from
								inheritedOverridingMember := Void
								pos := pMembers.count
							until
								pos = 0
							loop
								parentMember := pMembers.item (pos)
								if parentMember.version.name.is_equal (imoDsc.name) and then sameSignatures (parentMember.version, imoDsc) then
									debug
										o.putNL ("%T%TProcessed parent member `" + parentMember.out + "` as overriding while inheriting")
									end -- debug
									create inheritedOverridingMember.makeFromMember (parentMember)
									ioMembers.add (inheritedOverridingMember)
									if members.added (inheritedOverridingMember) then
										debug
											o.putNL ("%T%TOverriding member `" + inheritedOverridingMember.out + "` added")
										end -- debug
									else -- ???? Can this really happen ???
										debug
											o.putNL ("%T%TOverriding member `" + inheritedOverridingMember.out + "` was already added")
										end -- debug
									end -- if
									pMembers.delete (pos) -- remove such overrided member from the parent flat form
									pos := 0
								else
									pos := pos - 1
								end -- if
							end -- loop
							if inheritedOverridingMember = Void then
								-- version in parent not found
								o.putNL (
									"Incorrect overriding while inheriting `" + imoDsc.out + "` in unit `" + contextTypeDsc.fullUnitName + "`. " +
									"Parent `" + parent.contextTypeDsc.getUnitDeclaration.fullUnitName + "` does not have such member"
								)
								Result := True
							end -- if
						end -- if
						index := index - 1
					end -- loop
				else
					pMembers := parent.members -- Flat form of the parent
				end -- if
				from
					mIndex := pMembers.count
					debug
						o.putNL ("%T>Inheriting from parent `" + parent.contextTypeDsc.fullUnitName + "` with " + mIndex.out + " members")
					end -- debug
				until
					mIndex = 0
				loop
					parentMember := pMembers.item (mIndex)				
					debug
						o.putNL ("%T%TProcessing parent member `" + parentMember.out + "`") -- - " + parentMember.generator)
					end -- debug
					from
						toAddAsInherited := True
						index := members.count
					until
						index = 0
					loop
						member := members.item (index)
						debug
							o.putNL ("%T%TCurrent member `" + member.out + "` - " + member.generator)
						end -- debug
						if member.version.name.is_equal (parentMember.version.name) then
							if member.version.conformsTo (parentMember.version) then
								if member.isInheritedOverriding then
									-- Add into different origin&seed MST
									member := clone(member) 
									member.setSeedAndOrigin (parentMember.seed, parentMember.origin)
									if members.added (member) then
										debug
											o.putNL ("%T%TMember `" + member.out + "` added into another MST")
										end -- debug
									else
										debug
											o.putNL ("%T%TMember `" + member.out + "` was already added")
										end -- debug
									end -- if																		
								elseif member.isOverriding then
									debug
										o.putNL ("%T%TInherited member `" + parentMember.out + "` matches `" + member.out + "` which overrides it")
									end -- debug
									-- overriding in place check if signatures conform !
									if not ConformingSignatures (member.version, parentMember.version) then 
										o.putNL (
											"Non-conforming member overrding in unit `" + contextTypeDsc.fullUnitName + 
											"` member `" + member.version.fullMemberName +
											"` from parent `" + parent.contextTypeDsc.fullUnitName + "` and member `" + parentMember.version.fullMemberName + "`"
										)
										Result := True
									end -- if
									if member.seed = Void then
										member.setSeedAndOrigin (parentMember.seed, parentMember.origin)
										debug
											o.putNL ("%T%TMember `" + member.out + "` has got origin and seed")
										end -- debug
									elseif not member.seed.is_equal (parentMember.seed) then
										member := clone (member) 
										member.setSeedAndOrigin (parentMember.seed, parentMember.origin)
										if members.added (member) then
											debug
												o.putNL ("%T%TMember `" + member.out + "` added")
											end -- debug
										else
											debug
												o.putNL ("%T%TMember `" + member.out + "` was already added")
											end -- debug
										end -- if
									end -- if
									debug
										o.putNL (
											"%T%TInherited member `" + parentMember.out + 
											"` is overried with `" + member.out + "`"
										)
									end -- debug
									toAddAsInherited := False
								elseif theSameSignatures (member.version, parentMember.version) then
									-- identical signatures - versions clash ! Duplicating versions detected !!!
									o.putNL (
										"Duplicating member inherited in unit `" + contextTypeDsc.fullUnitName + 
										"` member `" + member.version.fullMemberName +
										"` from parent `" + parent.contextTypeDsc.fullUnitName + "`"
									)
									Result := True
									toAddAsInherited := False
								else
									-- valid overloading in place
									debug
										o.putNL ("%T%TInherited member `" + parentMember.out + "` matches `" + member.out + "` which overloads it")
									end -- debug
								end -- if
							else
							end -- if
						else
							-- Different name or overloading is name is the same
						end -- if
						index := index - 1
					end -- loop
					if toAddAsInherited then
						create inheritedMember.makeFromMember (parentMember)
						if members.added (inheritedMember) then
							debug
								o.putNL ("%T%TInherited member `" + parentMember.out + "` added as just inherited as is")
							end -- debug
						else
							debug
								o.putNL ("%T%TInherited member `" + parentMember.out + "` is already registred as just inherited")
							end -- debug
						end -- if
					end -- if
					mIndex := mIndex - 1
				end -- loop
				debug
					o.putNL ("%T<Processed parent `" + parent.contextTypeDsc.fullUnitName + "`")
				end -- debug			
				pIndex := pIndex - 1
			end -- loop
			from
				mIndex := members.count
			until
				mIndex = 0
			loop
				member := members.item (mIndex)
				if member.isOverriding and then member.seed = Void then
					o.putNL (
						"Unit member `" + member.version.fullMemberName +
						"` is incorrectly marked as overriding in unit `" + contextTypeDsc.fullUnitName + "`"
					)
					Result := True
				elseif ioCount > 0 then -- There are overrides while inheriting
					from
						index := ioCount
						check
							ioMembers /= Void
						end -- check
					until
						index = 0
					loop
						inheritedOverridingMember := ioMembers.item (index)
						if member /= inheritedOverridingMember and then member.version.name.is_equal (inheritedOverridingMember.version.name) then
							if inheritedOverridingMember.version.conformsTo (member.version) then
								-- Add into different origin&seed MST
								member.setVersionAndUnit (inheritedOverridingMember.version, inheritedOverridingMember.versionUnit)
							end -- if
						end -- if
						index := index - 1
					end -- loop	
				end -- if
				mIndex := mIndex - 1
			end -- loop
			debug
				o.putNL ("<<<Built flatform for `" + contextTypeDsc.fullUnitName + "`. It has " + members.count.out + " members")
			end -- debug
			flatFormBuilt := True
		end -- if
	end -- failedToBuildFlatForm
	
	out: String is
	local
		index: Integer
		i, n: Integer
		inheritedOverrides: Sorted_Array [InheritedMemberOverridingDescriptor]
		unitMembers: Sorted_Array [MemberDeclarationDescriptor]	
	do
		Result := "%T#" + id.out + "%T`" + contextTypeDsc.fullUnitName + "`"
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
		n := members.count
		if n > 0 then
			from
				i := 1
				Result.append_character (':')
			until
				i > n
			loop
				Result.append_character ('%T')
				Result.append_string (members.item(i).out)
				i := i + 1
			end -- loop	
		elseif False then
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

	sortMemebrsByOriginAndSeed is
	do
		if members.count > 0 then
			members.item (1).setSortByOriginAndSeed
			members.qsort
		end -- if
	end -- sortMemebrsByOriginAndSeed
		
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
	sortMode: SortMode is
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
class SortMode
feature {ContextUnit, MemberInVectorDescriptor}
	mode: Character
	setMode (m: Character) is
	do
		mode := m
	end -- setMode
end -- class SortMode

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

class InheritedOverridingMemberInVectorDescriptor
inherit
	MemberInVectorDescriptor
		redefine
			isOverriding, isInheritedOverriding
	end
creation
	makeFromMember
feature
	isOverriding: Boolean is True
	isInheritedOverriding: Boolean is True

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
end -- class InheritedOverridingMemberInVectorDescriptor

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

	setVersionAndUnit (v: like version; vu: like versionUnit) is
	require
		version_not_void: v /= Void
		version_unit_not_void: vu /= Void
	do
		version := v
		versionUnit := vu
	end -- setVersionAndUnit
	
	isOverriding: Boolean is
	do
		Result := version.isOverriding
	end -- isOverriding
	
	isInheritedOverriding: Boolean is
	once
	end -- isInheritedOverriding
	
	is_equal (other: like Current): Boolean is
	do
		inspect 
			sortMode.mode
		when defaultMode then
			Result := version.is_equal (other.version)
			if Result then
				if seed /= other.seed and then seed /= Void and then other.seed /= Void then
					Result := seed.is_equal (other.seed) and then origin.is_equal (other.origin)
				end -- if
			end -- if
		when idMode then
			Result := id = other.id
		when originAndSeedMode then
			if seed /= Void and then other.seed /= Void then
				check
					origin /= Void
				end -- check
				Result := seed.is_equal (other.seed) and then origin.is_equal (other.origin) and then version.is_equal (other.version)
			else
				Result := version.is_equal (other.version)
			end -- if
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
		when originAndSeedMode then
			if origin = Void then
				Result := other.origin /= Void
			elseif other.origin /= Void then
				check
					seed /= Void
					other.seed /= Void
				end -- check
				Result := seed < other.seed
				if not Result and then seed.is_equal (other.seed) then
					Result := version < other.version
				end -- if
			else			
				Result := version < other.version
			end -- if
		end -- inspect
	end -- infix "<"

	out: String is
	do
		if isOverriding then
			Result := "*"
		else
			Result := ""
		end -- if
		--Result := clone(version.fullMemberName)
		Result.append_string(version.fullMemberName)
		Result.append_character('@')
		Result.append_string(versionUnit.fullUnitName)
		Result.append_character('[')
		if seed = Void then
			Result.append_string("<Void>")
		else
			Result.append_string(seed.fullMemberName)
			Result.append_character('$')
			check
				origin /= Void
			end -- check
			Result.append_string(origin.fullUnitName)
		end -- if
		Result.append_character(']')		
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
		--		check
		--			origin /= Void
		--		end -- check
		--		Result.append_string(origin.fullUnitName)
		--	end -- if
		--end -- if
	end -- out
	setSortByID is
	do
		sortMode.setMode (idMode)
	end -- setSortByID

	setSortByOriginAndSeed is
	do
		sortMode.setMode (originAndSeedMode)
	end -- setSortByOriginAndSeed

	setSeedAndOrigin (aSeed: like seed; anOrigin: like origin) is
	require
		non_void_seed: aSeed /= Void	
		non_void_origin: anOrigin /= Void	
	do
		seed := aSeed
		origin := anOrigin
	end -- setSeed
feature {None}
	sortMode: SortMode is
	once
		create Result
	end -- sortMode
	init (aVersion: like version; aVersionUnit: ContextTypeDescriptor; aSeed: like seed; anOrigin: like origin) is
	require
		non_void_version: aVersion /= Void
		non_void_version_unit: aVersionUnit /= Void
		--non_void_seed: aSeed /= Void
	do
		versionUnit := aVersionUnit
		version := aVersion
		origin := anOrigin
		seed := aSeed
		sortMode.setMode(defaultMode)
	end -- init
	defaultMode:  Character is 'D'
	idMode: Character is '#'
	originAndSeedMode: Character is 'O'
invariant
	non_void_version: version /= Void
	non_void_version_unit: versionUnit /= Void
	--non_void_seed: seed /= Void
end -- class MemberInVectorDescriptor
