build hello => foo // exe entry point
	use Kernel // optional list of folders to search for required unit interfaces
end
foo do
   StandardIO.put ("Hello world!\n")
end