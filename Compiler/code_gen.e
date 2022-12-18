deferred class CodeGenerator
inherit
	Memory
		redefine
			dispose
	end
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	-- fileName refers to the name of the file which is to be created by code generator
	-- fileName has no extension it is to be added by the code generator
	-- if sucsesfull 1 is to be returned and -1 if error and 0 if code gneerator is not enabled yet
	-- if buildExecutable is false then dynamic and static libraries are to be created
	deferred
	ensure 
		-1 <= Result and then Result <= 1
	end -- genStart	
	dispose is
	do
		if ready then
			genEnd
			status := 0
		end -- if
	end -- dispose
	genEnd is
	-- should close the file created
	require
		code_generator_ready: ready
	deferred
	end -- genStart	
	genAssignmentToLocal () is
	require
		code_generator_ready: ready
	deferred
	end -- genAssignmentToLocal
	--genStaticAssignmentToAttribute () is
	--require
	--	ready: ready
	--deferred
	--end -- genStaticAssignmentToAttribute
	--genDynamicAssignmentToAttribute () is
	--require
	--	ready: ready
	--deferred
	--end -- genDynamicAssignmentToAttribute
	--genLabel (labelName: String) is
	--require
	--	ready: ready
	--	lable_not_void: labelName /= Void
	--deferred
	--end -- genLabel
	init (fileName: String; buildExecutable: Boolean) is
	require
		file_name_not_void: fileName /= Void
	do
		status := genStart (fileName, buildExecutable)
	end -- init
feature 
	status: Integer
	ready: Boolean is
	do
		Result := status = 1
	end -- ready
invariant
	valid_generator_status: -1 <= status and then status <= 1
end -- class CodeGenerator
class LLVM_CodeGenerator
inherit
	CodeGenerator
		rename init as cg_init
		export {None} cg_init
	end
create
	init
feature
	init (fileName, triplet: String; buildExecutable: Boolean) is
	require
		file_name_not_void: fileName /= Void
		triplet_not_void: triplet /= Void
	do
		cg_init (fileName, buildExecutable)
		setTriplet (triplet)
	end -- init

	setTriplet (triplet: String) is
	external "C" alias "llvm_setTriplet" 	
	end -- setTriplet

	genStart (fileName: String; buildExecutable: Boolean): Integer is
	external "C" alias "llvm_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "llvm_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	do
	--external "C" alias "llvm_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class LLVM_CodeGenerator
class MSIL_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	external "C" alias "msil_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "msil_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	do 
	--external "C" alias "msil_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class MSIL_CodeGenerator
class JVM_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	external "C" alias "jvm_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "jvm_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	do
	--external "C" alias "jvm_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class JVM_CodeGenerator
class MSIL_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	external "C" alias "msil_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "msil_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	do 
	--external "C" alias "msil_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class MSIL_CodeGenerator
class ARK_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	external "C" alias "ark_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "ark_genEnd"
	end -- genEnd
	genAssignmentToLocal () is
	do
	--external "C" alias "ark_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class ARK_CodeGenerator
class C_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String; buildExecutable: Boolean): Integer is
	-- fileName refers to the name of the file which is to be created by code generator
	-- fileName has extension !!!
	-- if succesfull 1 is to be returned and -1 if error and 0 if code generator is not enabled yet
	-- if buildExecutable is false then dynamic and static libraries are to be created
	local
		wasError: Boolean
	do
		if wasError then
			Result := -1
		else
			if fs.file_exists (fileName) then
				fs.remove_file (fileName)
			end -- if
			create file.make_create_read_write (fileName)
			
			hasEntryPoint := buildExecutable
			if buildExecutable then
				-- Generate entry point. 'main' is to be generated
				file.put_string ("int main(int argc, char **argv) {%N")
			else
				-- Library file is to be generated. No 'main'
			end -- if
			
			-- Not yet implemented
			
			Result := 1
		end -- if
	rescue
		wasError := True
		retry
	end -- genStart	
	genEnd is
	do
		if hasEntryPoint then
			-- Generate entry point. end of 'main' is to be generated
				file.put_string ("%Treturn 0;%N}%N")
		end -- if
		file.close
		file := Void
	end -- genEnd
	genAssignmentToLocal () is
	do
	end -- genAssignmentToLocal
feature {None}
	file: File
	hasEntryPoint: Boolean
	fs: FileSystem is
	once
		create Result
	end -- fs
end -- class C_CodeGenerator