separate class Producer
create
  init
feature
  consumer: Consumer
  produce 
  local
    I: Integer
  do
    from
	  I := 1
	until
      I > 10
    loop	  
      print("Producer writing ") print (I)
      сonsumer.consume(I) -- Так как сonsumer - это активный объект, то любой вызов его процедур асинхронный
	  I := I + 1
    end -- loop
  end -- produce
feature {None}
  init (c: like Consumer) do consumer := c end
end -- class Producer

separate class Consumer -- Для каждого активного объекта есть своя очередь вызовов на исполнение
feature
  consume (item: Integer) do
    print("Consumer read ") print (item)
  end -- Buf
end -- class Consumer

class Main 
create
  main
feature
  main
  local
     consumer: Consumer
	 producer: Producer
  do
    create consumer
	create producer.init (consumer)
    -- В программе всего 2 объекта с именами producer и consumer
    producer.produce
  end -- main
 end -- class Main
 