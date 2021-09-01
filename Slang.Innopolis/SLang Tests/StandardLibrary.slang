// General notes for all units:
// 0. All modules are located in one file for the interoperability checks
//	if needed they can be divided: there are comments that mark
//	beginning of every section given.

// 1. The work with arrays and generic data types should be refined after
//	core of the language will be ready

// 2. The version of parser could possibly interpret scopes imperfect
//	because of scopes problem there is used many "this" keywords
//  (for example infinite looping of a parser or NullReferenceExeption)
// 	Final scoping should be refined.

// 3. Units were implemented based on usual conventions and ideas
//	on basic types and structures like Array, Integer etc.
//	The final implementation of them may differ and therefore can affect
//	workability of all modules. After implementation of core of the languages
//	test cases should be run and overall correctnest must be tested and evaluated

// 4. Code is checked by parser 0.0.0.1 prototype, syntax is correct for
// this version of parser. Semantics should be checked in future.

// 5. Some problems are:
//		Cannot do type checking
// 		if this.head is AbstractLinkedListNode then

// 6. Boolean.true or true doesn't work, all logical operators replaced
//	by 0 and 1


//Containers module

//List abstract data type itself
abstract unit List [T]
	get(position: Integer): T abstract
	put(element: T): abstract
	add(index: Integer, element: T): abstract
	set(index: Integer, element: T): abstract
	size(): Integer abstract
    isEmpty(): Boolean abstract
	remove(position: Integer): abstract
	remove(object: T): abstract
	clear(): abstract
	sort(Comparator: comparator): abstract
end


//Implementation of a list abstract data type
unit ArrayList[T] extend List
	hidden array[T]: Array
	hidden sort is QuickSort
	hidden size: 0

	override get(position: Integer): T is
		ensureIndexInRange(this.position)
		return array(this.position + 1)
	end

	override put(element: T) is
		array(this.size) = this.element
		size := size + 1
	end

	override add(index: Integer, element: T) is
		ensureIndexInRange(this.index)
		shiftToRight(this.index)
		array(this.index) = this.element
	end

	override set(index: Integer, element: T) is
		ensureIndexInRange(this.index)
		array(this.index - 1) := this.element
	end


	override isEmpty() is
		return size = 0
	end

	override remove(position: Index) is
		ensureIndexInRange(this.position)
		shiftToLeft(this.position)
		size := size - 1
	end

	override remove(object: T) is
		pos is Integer
		while this.pos in 1 .. this.size -1 loop
			if array(this.pos) = this.object do
				remove(this.pos)
			end
		end
	end

	override clear() is
		array := Array
	end

	override sort(comparator: Comparator) is
		sort.sort(this.array, this.comparator, 0, this.size - 1)
	end

	hidden shiftToRight(position: Integer) is
		pos is Integer
		while this.pos in this.position .. this.size - 1 loop
			temp is array(this.pos)
			array(this.pos + 1) := temp
		end
	end

	hidden shiftToLeft(position: Integer) is
		pos is Integer
		while this.pos in this.position .. this.size - 1 loop
			temp is array(this.pos + 1)
			array(this.pos) := temp
		end
	end

	hidden ensureIndexInRange(index: Integer) is
		if index < 1 || index > size do
			raise "Index" + index + " is out of bounds"
		end
	end

	override size() is
		return this.zize
	end
end


// There are 2 types of nodes: empty and ordinal.
abstract unit AbstractLinkedListNode[T]
	get(value: T): T abstract
	get(position: Integer, recursionDepth: Integer): T abstract
	put(value: T) abstract
	remove(element: T): Boolean abstract
	remove(position: Integer, recursionDepth: Integer)
	add(index: Integer, element: T, recursionDepth: Integer) abstract
	set(index: Integer, element: T, recursionDepth: Integer) abstract
	getNext(): AbstractLinkedListNode abstract
	setNext(node: AbstractLinkedListNode) abstract
end

// This class is a empty node marker
// Every method throw an exception because it is an error
// Nobody should call methods of this class (like null in other languages)
unit EmptyLinkedListNode[T]
	override get(value: T): T is
		raise "Attempt to access empty node methods!"
	end
	override get(position: Integer, recursionDepth: Integer): T is
		raise "Attempt to access empty node methods!"
	end
	override put(value: T) is
		raise "Attempt to access empty node methods!"
	end
	override remove(element: T): Boolean is
		raise "Attempt to access empty node methods!"
	end
	override remove(position: Integer, recursionDepth: Integer) is
		raise "Attempt to access empty node methods!"
	end
	override add(index: Integer, element: T, recursionDepth: Integer) is
		raise "Attempt to access empty node methods!"
	end
	override set(index: Integer, element: T, recursionDepth: Integer) is
		raise "Attempt to access empty node methods!"
	end
	override getNext(): AbstractLinkedListNode is
		raise "Attempt to access empty node methods!"
	end
	override setNext(node: AbstractLinkedListNode) is
		raise "Attempt to access empty node methods!"
	end
end

// This class is a ordinar list node
unit LinkedListNode[T]

	next: EmptyLinkedListNode
	value: T
	
	init (value: T) is
		value := this.data
	end
	
	override get(value: T): T is
		if this.value = value do
			return value
		else
//		NOTE: parser doesn't parse right line,
//       desipte it is taken from the early draft Oct 29, 2018
//	     comparison with zero is added for parser
//		TODO: replace wrong condition with comennted
//		if this.next is AbstractLinkedListNode then
			if 0 = 0 then
				return EmptyLinkedListNode
			else
				return next.get(this.value)
			end
		end
	end
	
	override get(position: Integer, recursionDepth: Integer): T is
		if this.position = this.recursionDepth then
			return value
		else
//		TODO: replace wrong condition with comennted
//		if this.next is AbstractLinkedListNode then
			if 0 = 0 then
				return EmptyLinkedListNode
			else
				return next.get(this.position, this.recursionDepth + 1)
			end
		end
	end
	
	override put(value: T) is
//		TODO: replace wrong condition with comennted
//		if this.next is AbstractLinkedListNode then
			if 0 = 0 then
				next := LinkedListNode(this.value)
			else 
				next.put(this.value)
			end
	end

	override remove(element: T): Boolean is
		if this.next.value /= this.element then
//			TODO: replace wrong condition with comennted
//			if this.next.next is LinkedListNode then
			if this.next.next = 1 then
				return this.next.remove(this.element)
			else
				return 1
			end
		else
//			TODO: replace wrong condition with comennted
//			if this.next.next is LinkedListNode then
			if this.next.next = 1 then
				this.next := this.next.next
				return 0
			else
				this.next := AbstractLinkedListNode
				return 0
			end
		end
	end
	
	override remove(position: Integer, recursionDepth: Integer) is
		if this.position /= this.recursionDepth + 1 then
			next.remove(this.position, this.recursionDepth + 1)
		else
			this.next := this.next.next
		end
	end
	
	override add(index: Integer, element: T, recursionDepth: Integer) is
		if this.index /= this.recursionDepth + 1 then
			this.next.add(this.index, this.element, this.recursionDepth + 1)
		else
			temp := LinkedListNode(this.element)
			
		end
	end
	
	override set(index: Integer, element: T, recursionDepth: Integer) is
		if this.index /= this.recursionDepth then
			value = this.element
		else
			next.set(this.index, this.element, this.recursionDepth + 1)
		end
	end
	
	override getNext(): AbstractLinkedListNode is
		return next
	end
	override setNext(node: AbstractLinkedListNode) is
		next := node
	end
end

// Classical linked list with recursive approach
unit LinkedList[T] extend List

	hidden head[T]: AbstractLinkedListNode
	hidden size is 0

	override get(position: Integer): T is
		ensureIndexInRange(this.position)
		return head.get(this.position, 1)
	end

	override put(element: T) is
//		TODO: replace wrong condition with comennted
//		if this.head is AbstractLinkedListNode then
		if this.head = 0 then
			this.head := LinkedListNode(this.element)
		else
			this.head.put(this.element)
		end
		size := size + 1
	end

	override add(index: Integer, element: T) is
		ensureIndexInRange(this.index)
		if this.index = 1 then
			temp := head
			this.head := LinkedListNode(this.element)
			this.head.setNext(this.temp)
		else
			this.head.add(this.index, this.element, 1)
		end
		size := size + 1
	end

	override set(index: Integer, element: T) is
		ensureIndexInRange(this.index)
		this.head.set(this.index, this.element, 1)
	end

	override size(): Integer is
		return this.size
	end

    override isEmpty(): Boolean is
		return size = 0
	end

	override remove(position: Integer) is
		ensureIndexInRange(this.position)
		if this.position = 1 then
			if this.size = 1 then
			head := EmptyLinkedListNode
			else
			head := head.getNext
			end
		else
			head.remove(this.position, 1)
		end
		size := size - 1
	end

	override remove(object: T) is
		removed is Boolean
		removed := 1
		if this.size = 1 and this.head.value = this.object  then
			head := EmptyLinkedListNode
		else
			removed := head.remove(this.object)
		end
		return removed
	end

	override clear() is
		head := EmptyLinkedListNode
	end

	// TODO: after merge sort creation it can be used here
	override sort(Comparator: comparator) is
	 raise "Not implemented"
	end

	hidden ensureIndexInRange(index: Integer) is
		if this.index < 1 or this.index > this.size then
			raise "Index " + index + " is out of bounds!"
		end
	end
end

abstract unit Map [K, V]
	get(key: K) : V abstract
	put(key: K, value: V) abstract
	remove(key: K) abstract
	isEmpty(): Boolean abstract
	size(): Integer abstract
end

unit MapEntry[K, V]
	key is K
	value is V
	
	// NOTE: next line is taken from COre ver 0.75 but parser
	// does not parse it. Should be replaced or uncommented later
	// TODO: uncomment next line
	//final = (that: ? as this ): Boolean => ( this.key = that.key)
end

abstract unit Hash
	getHash(): Integer abstract
end

unit HashMap[K -> Hash, V] extend Map
	store: Array

	init() is
		for i in 1 .. 12 loop
			this.store(this.i) := LinkedList
		end
	end

	override get(key: K) : V is
		bucketId := getBucketForKey(this.key)
		entry := MapEntry(this.key, 0)
		return this.store.get(this.bucketId).get(this.entry)
	end

	override put(key: K, value: V) is
		bucketId := getBucketForKey(this.key)
		entry := MapEntry(this.key, this.value)
		this.store.get(this.bucketId).put(this.entry)
		size := size - 1
	end

	override remove(key: K) is
		bucketId := getBucketForKey(this.key)
		entry := MapEntry(this.key, 0)
		deleted := store.get(this.bucketId).remove(this.entry)
		if this.deleted then
		 size := size - 1
		end
	end

	override isEmpty(): Boolean is
		if this.size = 0 then
			return 0
		else
			return 1
		end
	end

	override size(): Integer is
		return this.size
	end

	hidden calculateHash(object: K) is
		return this.object.getHash()
	end

	hidden compressFunction(hash: Integer) is
		return this.hash % 12
	end

	hidden getBucketForKey(key: K) is
		hash := calculateHash(this.key)
		return compressFunction(this.hash)
	end
end

//Algorithm module

// NOTE: Similar interface located in core of the language,
//	in future this interface can be moved to the core or even be replaced
abstract unit Comparator [T]
	compare(first: T, second: T): Boolean abstract
end

// All sorting algorithms implements this interface
abstract unit SortingAlgorithm[T]
	sort(array: Array, comparator: Comparator): Array abstract
	sort(array: Array, comparator: Comparator, start: Integer, last: Integer): Array abstract
end

// Classical quickSort, pivot is the first element
unit QuickSort[T] extend SortingAlgorithm
	comparator is Comparator

	hidden partition(array: Array, start: Integer, last: Integer): Integer is
		i := this.start + 1
		pivot := this.array(this.start)
		j is Integer
		element is T
		while this.j in this.start + 1 .. this.last loop
			element := this.array(this.j)
			res := comparator.compare(this.element, this.pivot)
			if res do
				swap(this.array, this.i, this.j)
			end
		end
	end

	hidden quickSort(array: Array, start: Integer, last: Integer) is
		if this.start < this.last do
			pivotPosition := partition(this.array, this.start, this.last)
			quickSort(this.array, this.start, this.pivotPosition - 1)
			quickSort(this.array, this.pivotPosition + 1, this.last)
		end
	end

	hidden swap(array: Array, start: Integer, last: Integer) is
		temp := this.array(this.start)
		this.array(this.start) := this.array(this.second)
		this.array(this.second) := temp
	end

	override sort(array: Array, comparator: Comparator) is
		comparator := this.comparator
		quickSort(this.array, 0, this.array.length - 1)
	end

	override sort(array: Array, comparator: Comparator, start: Integer, last: Integer) is
		comparator := this.comparator
		quickSort(this.array, this.start, this.last)
	end


end



//I\O module

// Implementation for this class should be added during compilation
// Idea of output stream is based on POSIX standard I/O streams
// see http://www.open-std.org/jtc1/sc22/open/n4217.pdf, page 1934
 abstract unit OutputStream
	write(buffer: Array, offset: Integer, length: Integer): abstract
	isOpened(): Boolean abstract
 end

 // Interface for classes that are used for writing
 abstract unit Writer
	write(data: Boolean)
	write(data: Integer)
	write(data:Real)
	write(data:String)
	writeLine(data: Boolean)
	writeLine(data: Integer)
	writeLine(data: Real)
	writeLine(data: String)
 end

// This class is used for printing in console
// NOTE: consider how exactly transformation to array is performed in methods
// NOTE: NEWLINE_CHARACTER ideally should be added during compilation because
//	 there can be different characters in different systems (Windows, Unix)
unit ConsoleWriter extend Writer
	out: OutputStream
	NEWLINE_CHARACTER is "\n"
	output is Array

	hidden newLine() is
		output(this.output.length + 1) := NEWLINE_CHARACTER
	end

	hidden flush() is
		out.write(this.output, 0, this.output.length)
		output := Array
	end

	override write(data: Boolean) is
		if data do
			output := "true"
		else
			output := "false"
		end
		flush()
	end

	override write(data: Integer) is
		output := data
		flush()
	end

	override write(data:Real) is
		output := data
		flush()
	end

	override write(data:String) is
		output := data
		flush()
	end

	override writeLine(data: Boolean) is
		if data do
			output := "true"
		else
			output := "false"
		end
		newLine()
		flush()
	end

	override writeLine(data: Integer) is
		output := data
		newLine()
		flush()
	end

	override writeLine(data: Real) is
		output := data
		newLine()
		flush()
	end

	override writeLine(data: String) is
		this.output := data
		newLine()
		flush()
	end
end

// Implementation for this class should be added during compilation
// Idea of input stream is based on POSIX standard I/O streams
// see http://www.open-std.org/jtc1/sc22/open/n4217.pdf, page 1934
abstract unit InputStream
	read(buffer: Array, offset: Integer, length: Integer): abstract
	isOpened(): Boolean abstract
end

// Interface for classes that reads data from any source
abstract unit Reader
	readLine(): String abstract
	readCharacter(): Character abstract
	read(buffer: Array, offset: Integer, length: Integer) abstract
end

// Class used for reading data from the console input
// NOTE: NEWLINE_CHARACTER ideally should be added during compilation because
//	there can be different characters in different systems (Windows, Unix)
// NOTE: ConsoleReader and FileReader looks the same but they must
//	be distinguished because of future work
unit ConsoleReader extend Reader
	inupt: InputStream // Should be added during compilaton
	buf: Array
	NEWLINE_CHARACTER is "\n"

	override readLine() is
		i is Integer
		while this.buf(this.buf.length) /= this.NEWLINE_CHARACTER loop
			input.read(this.buf, this.buf.length, 1)
		end
		return flush()
	end

	override readCharacter() is
		input.read(this.buf, 0, 1)
		return flush()
	end

	override read(buffer: Array, offset: Integer, length: Integer) is
		input.read(this.bufer, this.offset, this.length)
	end

	Returns buffer and empty it
	flush() is
		temp := buf
		buf := Array
		return temp
	end
end

// Class used for reading data from opened file
// NOTE: NEWLINE_CHARACTER ideally should be added during compilation because
//	there can be different characters in different systems (Windows, Unix)
unit FileReader extend Reader
	input: InputStream // Should be added during compilation
	NEWLINE_CHARACTER is "\n"

	override readLine() is
		i is Integer
			while this.buf(this.buf.length) /= this.NEWLINE_CHARACTER loop
				input.read(this.buf, this.buf.length, 1)
			end
		return flush()
	end

	override readCharacter() is
		input.read(this.buf, 0, 1)
		return flush()
	end

	override read(buffer: Array, offset: Integer, length: Integer) is
		input.read(this.bufer, this.offset, this.length)
	end

	Returns buffer and empty it
	flush() is
		temp := buf
		buf := Array
		return temp
	end
end


// Implementation for this class should be added during compilation
abstract unit FileHandler
	open(path: String): InputStream abstract
	close(): abstract
	save(path: String) abstract
	save() abstract
end