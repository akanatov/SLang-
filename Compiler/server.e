class Server
feature
	fs: FileSystem is
	once
		create Result
	end -- fs

	unitAnyDsc: UnitTypeNameDescriptor is
	once
		create Result.init (anyName, Void)
	end -- unitAnyDsc

	anyName: String is "Any"
end -- class Server
