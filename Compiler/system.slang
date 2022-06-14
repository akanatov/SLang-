build Test1 => foo // exe entry point, if not specfied lib // dll file toi be created
	use Kernel Util "c:\path\path\**" // optional list of folders to search for required unit interfaces
	foreign "..\..\startup.obj" // optional list of external libaries to be linked with this output
end
build Test2: "."
	use Kernel Util "c:\path\path\**" // optional list of folders to search for required unit interfaces
	foreign "..\..\startup.obj" // optional list of external libaries to be linked with this output
end
