active type Producer
  produce do
    while I in 1..10 do
      IO.put("Producer writing", I)
      // Имя типа задает синглетон - т.е. один обхект (companion object, прообраз static)
      Consumer.Buf(I) // Так как Consumer - это активный объект, то любой вызов его процедур асинхронный
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
