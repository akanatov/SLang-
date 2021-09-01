use TestSupport  

title is "Generic parameters in functions"
section is "generics.functions" 
topic is "Describe functions with generic parameters"

// Assuming that all units are based on some unit withthod toString which used by StandardIO library
log[T](obj: T) is
	StandardIO.put(obj)
end