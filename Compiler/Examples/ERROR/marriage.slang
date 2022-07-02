unit Person
	name: String
	spouse: ? as this {} := (aSpouse: as spouse) do // Setter is visible only for this unit and its descendants
		spouse := aSpouse
	end // spouse setter
	{} clearSpouse // This routine is available only for unit Person and its descendants
	require
		isMarried /// Person should be married!!!
	do 
		? spouse
	ensure
		not isMarried
	end // clearSpouse
	safe isMarried: Boolean => spouse is Person
	marry (other: as this) 
		/// merry Mary marry me :-)
	require
		not isMarried() /// I am not married
		not other.isMarried() /// My spouse not married
	do 
		spouse := other
		do {other} // other is to be valid after we leave the block
			other.spouse := this
		end
	ensure
		isMarried() /// I am married
		other.isMarried() /// My spouse is married
	end
	divorce
	require
		isMarried() /// I am married
		other.isMarried() /// My spouse is married
	do 
		spouse.clearSpouse () 
		? spouse
	ensure
		not isMarried() /// I am not married any longer
		not other.isMarried() /// My spouse is not married any longer
	end
	Person (:= name) do
	end
require
	isMarried() implies spouse.spouse = this
end // Person


// Example code (anonymous routine), which illustrates the concept
husband is Person ("Ivan")
wife is Person("Maria")
/*
do {husband, wife} // kind of critical section to omit checking invariants for entities listed in {}
	husband.marry (wife)
	wife.marry(husband)
ensure // Now both objects are consistent
	husband.spouse = wife
	wife.spouse = husband
end

while AllIsOk do
	if oneSpouseDies do
		break
	else if AdulterOccurred do
		break
	end
end // while

do {husband, wife}
	husband.divorce ()
end

/*unit PolygamyPerson
	// TBD :-)
end */
*/