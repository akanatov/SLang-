class Server
feature
	fs: FileSystem is
	once
		create Result
	end -- fs

	unitAnyDsc: UnitTypeNameDescriptor is
	once
		create Result.init ("Any", Void)
	end -- unitAnyDsc

end
