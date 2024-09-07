with Ada.Text_IO; use Ada.Text_IO;

procedure PC_Rendezvous is
   task Producer;
   task Consumer is
      entry Buf(Item : in Integer);
   end Consumer;
   task body Producer is
   begin
      for I in 1..10 loop
         Put_Line("Producer writing" & Integer'Image(I));
         Consumer.Buf(I);
      end loop;
   end Producer;
   task body Consumer is
      Temp : Integer;
   begin
      loop
         select
            accept Buf(Item : in Integer) do
               temp := Item;
            end;
            Put_Line("Consumer read" & Integer'Image(Temp));
         or
            terminate;
         end select;
      end loop;
   end Consumer;

begin
   null;
end PC_Rendezvous;