unit DynamicObject
	final call (rtn_name: String; args: ()) do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr is ProcedureDescriptor do
			call (rtn_descr, args)
		else
			raise "Call to non-existing procedure '" + rtn_name + "'"
		end			
	end
end
