deferred class CodeGenerator
inherit
	Memory
		redefine
			dispose
	end
feature
	genStart (fileName: String): Boolean is
	-- fileName refers to the name of the file which is to be created by code generator and if sussecfull Ture is to be returned and false otherwise
	deferred
	end -- genStart	
	dispose is
	do
		if ready then
			ready := False
			genEnd
		end -- if
	end -- dispose
	genEnd is
	-- should close the file created
	deferred
	end -- genStart	
	genAssignmentToLocal () is
	require
		ready: ready
	deferred
	end -- genAssignmentToLocal
	--genAssignmentToAttribute () is
	--require
	--	ready: ready
	--deferred
	--end -- genAssignmentToStaticAttribute
	--genAssignmentToDynamicAttribute () is
	--require
	--	ready: ready
	--deferred
	--end -- genAssignmentToDynamicAttribute
	--genLabel (labelName: String) is
	--require
	--	ready: ready
	--	lable_not_void: labelName /= Void
	--deferred
	--end -- genLabel
	init (fileName: String) is
	require
		file_name_not_void: fileName /= Void
	do
		ready := genStart (fileName	)
	end -- init
feature {None}
	ready: Boolean
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
	init (fileName, triplet: String) is
	require
		file_name_not_void: fileName /= Void
		triplet_not_void: triplet /= Void
	do
		cg_init (fileName)
		setTriplet (triplet)
	end -- init

	setTriplet (triplet: String) is
	external "C" alias "llvm_setTriplet" 	
	end -- setTriplet

	genStart (fileName: String): Boolean is
	external "C" alias "llvm_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "llvm_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	external "C" alias "llvm_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class LLVM_Windows_CodeGenerator
class MSIL_CodeGenerator
inherit
	CodeGenerator
	end
create
	init
feature
	genStart (fileName: String): Boolean is
	external "C" alias "msil_genStart" 	
	end -- genStart	
	genEnd is
	external "C" alias "msil_genEnd"
	end -- genStart	
	genAssignmentToLocal () is
	external "C" alias "msil_genAssignmentToLocal"
	end -- genAssignmentToLocal
end -- class MSIL_CodeGenerator