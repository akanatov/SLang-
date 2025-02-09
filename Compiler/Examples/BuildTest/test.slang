build Library from "."
end

build ExecutableWithRoutineEntry entry main
end

build ExecutableWithUnitEntry entry Root
end

// Anonymous routine(script) code
root is Root

type Root
	Root do end  // Init procedure (constructor) with no parameters
end

type Any
end

// Standalone routine main
main do
end

