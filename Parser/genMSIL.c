// MSIL generation primitives to be implemented by Zouev
/*	genStart (fileName: String; buildExecutable: Boolean): Boolean is
	-- fileName refers to the name of the file which is to be created by code generator
	-- fileName has no extension it is to be added by the code generator
	-- if sucsesfull 1 is to be returned and -1 if eror and 0 if code gneerator is not enabled yet
	-- if buildExecutable is false then dynamic and static libraries are to be created
	*/
int msil_genStart (char* fileName, char buildExecutable) {
	return 0;
}
void msil_genEnd () {
}
