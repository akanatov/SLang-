build Test1 entry foo // exe entry point, if not specfied lib // dll file to be created
	cluster Kernel Util "c:\path\path\**" // optional list of folders to search for required type interfaces
	foreign "..\..\startup.obj" // optional list of external libaries to be linked with this output
end
build Test2 from "."
	cluster Kernel Util "c:\path\path\**" // optional list of folders to search for required type interfaces
	foreign "..\..\startup.obj" // optional list of external libaries to be linked with this output
end
