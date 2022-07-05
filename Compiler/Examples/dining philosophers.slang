philosophers is (Philosopher ("Aristotle"), Philosopher ("Kant"), Philosopher ("Spinoza"), Philosopher ("Marx"), Philosopher ("Russell"))
forks is (Fork (1), Fork (2), Fork (3), Fork (4), Fork (5))
require
   philosophers.count = forks.count or else philosophers.count = 1 and then forks.count = 2
   /* Task is valid, if # of forks is eual to the # of philosophers or if there is only 1 philosopher then # of froks is equal to 2*/
do end
while true do /// Let them eat forever. Other algorithms may be applied
   while seat in philosophers.lower .. philosophers.upper do
      StandardIO.put ("Philosopher '" + philosophers (seat).name + "' is awake for lunch\n")
      eat (philosophers (seat), forks (seat), forks (if seat = philosophers.upper do forks.lower else seat + 1))
   end
end
eat (philosopher: Philosopher; left, right: Fork) do
   /* Procedure with 3 concurrent parameters. Every call to eat creates a critical section which is parameterized by required resources to enter it. When all resources are captured then the call is being made having all resources in the exclusive access within the procedure */
   StandardIO.put ("Philosopher '" + philosopher.name + "' is eating with forks #" + left.id + " and #" + right.id + "\n")
end
concurrent  unit  Philosopher 
   name: String
   Philosopher (:= name) none
end
concurrent  unit  Fork
   id: Integer
   Fork (:= id) none
end
