abstract type  Comparable 
	pure < (other: as this ): Boolean abstract
	override pure = (other: as this ): Boolean => not (this < other) and not (other < this)
	pure > (other: as this ): Boolean => other < this
	pure <= (other: as this ): Boolean => not (other < this)
	pure >= (other: as this ): Boolean => not (this < other)
	pure max (other: as this ): as this => if this > other do this else other
	pure min (other: as this ): as this => if this < other do this else other
require
	not ( this < this ) /// Irreflexive comparison
end // Comparable
