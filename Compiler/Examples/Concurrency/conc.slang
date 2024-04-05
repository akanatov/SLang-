/* 
1. Every active type has a queue of activation requests per every 'exported' routine
2. All active type attrubutes are ensured being atomic for all read-write requests
*/
active type Producer[G]
  init (:= queue) none
  produce (generateData: rtn: G) do
    while not toStop do
      queue.put (generateData)
    end
  end
  stop do
    toStop := true
  end
  {}:
  queue: Queue [G]
  toStop: Boolean
end
active type Consumer[G]
  init (:= queue) none
  consume (processData: rtn (G)) do
    isProcessing := true
    while not toStop do
      processData (queue.get)
    end
    // If there are some non-processed elements left - process them
    while not queue.isEmpty do
      processData (queue.get)
    end 
    isProcessing := false
  end
  stop do
    toStop := true
  end
  isProcessing: Boolean
  {}:
  toStop: Boolean
  queue:  Queue [G]
end
active type Queue [G]
  put (element: G) do
    queue.resize (queue.count + 1, element)    
  end
  get: G
  require
    queue.count > 0 // while waiting for precondition be true other type routines can be called
  do
    return := queue (queue.upper)
    queue.resize (queue.count - 1)
  end
  init do
    queue is () // Empty array
  end
  isEmpty: Boolean => queue.count = 0
  {} queue: Array [G]
end

// Example how to work with producer and consumer
type Data // Unit which shoudk contain some data which will be produced and consumed
end
timeToExit: Boolean do // Function which determines if it is the time to shut the whole system
	return if Timer.totalTimeOfExecution > 1000 do true else false
end
q is Queue [Data] // create queue
p is Producer [Data] (q) // create producer
c is Consumer [Data] (q) // create consumer
p.produce (rtn : Data => Data) // start producing Data objects
c.consume (rtn (data: Data) do IO.print (data) end) // start processing Data objects
var exit: Boolean
while not exit do
  exit := timeToExit // Just do some activity of its own - main thread of the application
end
// Time to ramp down the whole system
p.stop // Stop producing
c.stop // Stop consuming
while c.isProcessing do // As there could be still some elements in the queue for processign ensure all are processed
  // wait a bit to process all elements ...
  Timer.delay (1)
end
// Here we have all stopped and processed - BYE!




