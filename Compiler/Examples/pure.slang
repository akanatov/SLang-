 type  X
{this} fieldIndex (fieldNAme: String): Integer
foreign
ensure
return in 0 .. count /// If no such field then 0 otherwise valid position
end // fieldIndex

pure count: Integer do end
end

