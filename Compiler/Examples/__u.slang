// How to deal with dynamic objects .... WIP !!!
type DynamicObject
	/* This type allows to deal with dynamically loaded/created objects 
	providing dynamic type checking.
	*/
	// Access members by name
	final call (rtn_name: String; args: ()) do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr type ProcedureDescriptor do
			call (rtn_descr, args)
		else
			raise "Call to non-existing procedure '" + rtn_name + "'"
		end			
	end
	final call (rtn_name: String; args: ()): Any do
		rtn_descr is findRoutine(rtn_name)
		if rtn_descr type FunctionDescriptor do
			return call (rtn_descr, args)
		else
			raise "Call to non-existing function '" + rtn_name + "'"
		end			
	end
	final get_value (attr_name: String): Any do
		attr_descr is findAttribute (attr_name)
		if attr_descr type AttributeDescriptor do
			return attr_descr.value
		else
			raise "Access to non-existing attribute '" + attr_name + "'"
		end
	end
	final set_value (attr_name: String; value: Any) do
		attr_descr is findAttribute (attr_name)
		if attr_descr type VariableDescriptor do
			attr_descr.value := value
		else
			raise "Access to non-existing/writable attribute '" + attr_name + "'"
		end
	end
	// Access members by object (address) 
	final routines() : Array [RoutineDecsriptor] foreign
	final attributes (): Array [AttributeDescriptor] foreign
	final call (rtnDsc: ProcedureDescriptor; args: ()) foreign
	final call (rtnDsc: FunctionDecsriptor; args: ()): Any foreign
	final get_value (attr: AttributeDescriptor): Any => attr.value
	final set_value (attr: VariableDescriptor; value: Any) foreign
	{this} final findRoutine (rtn_name: String): ?RoutineDescriptor foreign
	{this} final findAttribute (attr_name: String): ?AttributeDescriptor foreign
	DynamicObject do
		/// Create empty dynamic object
	end
	DynamicObject (fileName: String) do
		/// Load dynamic object from file
		raise "Not implemented yet!!!"
	end
	DynamicObject (bytes: Array [Byte]) foreign
		/// Create dynamic object from binary data sequence
	DynamicObject (other: Any) foreign
		/// Create dynamic object from any other object
	
end

virtual type MemberDescriptor
	final name: String
end
virtual type RoutineDescriptor extend MemberDescriptor
	final arguments: Array [ArgumentDescriptor]
end // RoutineDescriptor

final type ProcedureDescriptor extend RoutineDescriptor
	() alias apply (args: ()) foreign
		// That is a procedure call
	ProcedureDescriptor (aName: as name; args: as arguments) do
		name is aName
		arguments is args
	end
end // Procedure [Arguments -> ()]

final type FunctionDescriptor extend RoutineDescriptor
	fType: TypeDescriptor
	() alias apply (args: Arguments): Any foreign
		// That is a function call
	FunctionDescriptor (aName: as name; args: as arguments; aType: as fType) do
		name := aName
		arguments := args
		fType := aType
	end
end // Function [Arguments -> (), Result]*/

virtual type AttributeDescriptor extend MemberDescriptor
	value: Any
	aType: TypeDescriptor
end // AttributeDescriptor

final type VariableDescriptor  extend AttributeDescriptor 
	override := alias set (v: as value) do 
		value := v
	end
	VariableDescriptor (aName: as name; a_type: as aType; aValue: as value) do
		name is aName
		aType is a_type
		value is aValue
	end
end // VariableDescriptor

final type ConstantDescriptor  extend AttributeDescriptor 
	ConstantDescriptor (aName: as name; aType: as cType; aValue: as value) do
		name is aName
		cType is aType
		value is aValue
	end
end // ConstantDescriptor

final type TypeDescriptior
	final name: String
	final routines : Array [RoutineDescriptor]
	final attributes : Array [AttributeDescriptor]
	final parents: Array [TypeDesciptor]
	TypeDescriptior (aName: as name; prtns: as parents; rtns: as routines; attrs: as attributes) do
		name is aName
		parents is prtns
		routines is rtns
		attributes is attrs
	end
end
