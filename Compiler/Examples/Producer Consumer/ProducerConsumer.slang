active type Producer
  produce do
    while i in 1..10 do
      IO.put("Producer writing", i)
      // Имя типа задает синглетон - т.е. один объект (companion object, прообраз static)
      Consumer.consume(i) // Так как Consumer - это активный объект, то любой вызов его процедур асинхронный
    end // while
  end // produce
end // Producer
active type Consumer // Для каждого активного объекта есть своя очередь вызовов на исполнение
  consume (item: Integer) do
    IO.put("Consumer read", item)
  end // consume
end // Consumer

// В программе всего 2 объекта с именами Producer и Consumer
Producer.produce
