abstract type NamedPerson
	name: String
	{} clearSpouse () // This routine is available only for type NamedPerson and its descendants
	require
		isMarried /// Person should be married!!!
	abstract
	ensure
		not isMarried
	end // clearSpouse
	safe isMarried (): Boolean abstract
	marry (other: as this) 
		/// merry Mary marry me :-)
	require
		not isMarried() /// I am not married
		not other.isMarried() /// My spouse not married
	abstract
	ensure
		isMarried() /// I am married
		other.isMarried() /// My spouse is married
	end // marry
	divorce ()
	require
		isMarried() /// I am married
		other.isMarried() /// My spouse is married
	abstract
	ensure
		not isMarried() /// I am not married any longer
		not other.isMarried() /// My spouse is not married any longer
	end // divorce
end // NamedPerson

type MonogamousPerson extend NamedPerson
	spouse: ? as this {} := (aSpouse: as spouse) do // Setter is visible only for this type and its descendants
		spouse := aSpouse
	end // spouse setter
	{} override clearSpouse ()
	do 
		? spouse
	end // clearSpouse
	override safe isMarried (): Boolean => spouse is Person
	override marry (other: as this) 
	do 
		spouse := other
		do {other} // other is to be valid after we leave the block
			other.spouse := this
		end // do
	end // marry
	override divorce ()
	do 
		spouse.clearSpouse () 
		? spouse
	end // divorce
	MonogamousPerson (:= name) do end // MonogamousPerson constructor
require
	isMarried() implies spouse.spouse = this
end // MonogamousPerson

type PolygamyPerson extend NamedPerson
	spouses: Array [as this]
	{} override clearSpouse ()
	do 
		spouses.resize (1, 0) // :-)
	end // clearSpouse
	override safe isMarried (): Boolean => spouses.count > 0
	override marry (other: as this) 
	do 
		spouses.force (other, spouse.upper + 1)
		do {other} // other is to be valid after we leave the block
			other.spouse := this
		end
	end // marry
	override divorce ()
	do 
		spouse.clearSpouse () 
		spouses.resize (1, 0) // :-)
	end // divorce
end // PolygamyPerson


// Example code (anonymous routine), which illustrates the concept
husband is MonogamousPerson ("Ivan")
wife is MonogamousPerson ("Maria")

do {husband, wife} // kind of critical section to omit checking invariants for entities listed in {}
	husband.marry (wife)
	wife.marry(husband)
ensure // Now both objects are consistent
	husband.spouse = wife
	wife.spouse = husband
end

marrageStatus is "Married!"
while familyLifeOK do
	if oneSpouseDies do
		raise 1
	elseif adulterOccurred do
		raise 2
	end
when 1 do
	marrageStatus := "Marrage is over due to spouse death"
when 2 do
	marrageStatus := "Marrage is over due to adulter"
end // while

do {husband, wife}
	husband.divorce ()
ensure // Now both objects are consistent
	not husband.isMarried
	not wife.isMarried
end // do


