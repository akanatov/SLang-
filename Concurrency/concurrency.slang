concurrent unit Queue[G]
	put (e: G) do
		if buffer.count = 0 do
			first := 1
			last := 1
		else
			last := last + 1
		end
		buffer(last) := e // if last greater than biffer.count dynamic arrays grows to accomodate element e at position last
	end // put
	get: G 
	require
		canGet
	do
		return := buffer(first)
		if first = last do
			first := 0			
			last := 0
		else
			first := first + 1
		end
	end // get
	pure canGet: Boolean => first > 0
	Queue do
		buffer is ()
		first is 0
		last is 0
	end
	{}:
	buffer: Array [G]
	first,
	last: Cardinal
require
	first <= last
	buffer.count = 0 implies first = 0
end // Queue

unit Data end

concurrent unit Producer
	produce do
		while true do
			Queue[Data].put (Data) // all calls to put are put into the call queue for the object Queue
		end
	end
end

concurrent unit Consumer
	consume do
		while true do
			consumeData (Queue[Data].get) // all calls to get are puit into the queue for the object Queue
		end
	end
	pure consumeData (d: Data) do
		// Do something with the data
	end
end

// Main program
Producer.produce // async call
Consumer.consume // async call

while true do
end

