with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   task Producer;
   task Consumer is
      entry consume(Item : in Integer);
   end Consumer;
   task body Producer is
   begin
      for I in 1..10 loop
         Put_Line("Producer writing " & Integer'Image(I));
         Consumer.consume(I);
      end loop;
   end Producer;
   task body Consumer is
      Temp : Integer;
   begin
      loop
         select
            accept consume(Item : in Integer) do
               Temp := Item;
            end;
            Put_Line("Consumer read " & Integer'Image(Temp));
         or
            terminate;
         end select;
      end loop;
   end Consumer;

begin
   null;
end Main;