--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body Task_States is

   -- Shared Task State --

   protected body Task_State_Object is

      -- Reference Counting --

      procedure Acquire is
      begin
         Num_References := Num_References + 1;
      end Acquire;

      procedure Release (Count : out Refcount) is
      begin
         Num_References := Num_References - 1;
         Count := Num_References;
      end Release;


      -- Enter/Exit Foreground --

      procedure Enter_Foreground is
      begin
         Test_Cancelled;

         Foreground := True;
      end Enter_Foreground;

      procedure Exit_Foreground is
      begin
         Foreground := False;
         Test_Cancelled;
      end Exit_Foreground;


      -- Cancellation Testing --

      procedure Test_Cancelled is
      begin
         if Is_Cancelled then

            if not After_Cancel.Is_Empty then
               After_Cancel.Element.Continue;
               After_Cancel.Clear;
            end if;

            raise Task_Cancelled;
         end if;
      end Test_Cancelled;

      function Is_Cancelled return Boolean is
        (Cancelled);


      -- Cancellation --

      procedure Cancel is
      begin
         Cancelled := True;
      end Cancel;

      procedure Cancel (C : Continuation'Class) is
      begin
         Cancel;

         if not Foreground then
            C.Continue;

         else
            After_Cancel := Continuation_Holders.To_Holder(C);

         end if;
      end Cancel;

   end Task_State_Object;


   -- Constructor --

   function Create return Task_State is
      (Controlled with Object => new Task_State_Object);


   -- Memory Management --

   procedure Finalize (State : in out Task_State) is
      Count : Refcount;

   begin
      if State.Object /= null then
         State.Object.Release(Count);

         if Count = 0 then
            Free(State.Object);
            State.Object := null;
         end if;
      end if;
   end Finalize;

   procedure Adjust (State : in out Task_State) is
   begin
      if State.Object /= null then
         State.Object.Acquire;
      end if;
   end Adjust;

   function Is_Empty (State : Task_State) return Boolean is
     (State.Object = null);


   -- Wrapper Functions --

   procedure Enter_Foreground (State : Task_State) is
   begin
      State.Object.Enter_Foreground;
   end Enter_Foreground;

   procedure Exit_Foreground (State : Task_State) is
   begin
      State.Object.Exit_Foreground;
   end Exit_Foreground;

   procedure Test_Cancelled (State : Task_State) is
   begin
      State.Object.Test_Cancelled;
   end Test_Cancelled;

   function Is_Cancelled (State : Task_State) return Boolean is
   begin
      return State.Object.Is_Cancelled;
   end Is_Cancelled;

   procedure Cancel (State : Task_State) is
   begin
      State.Object.Cancel;
   end Cancel;

   procedure Cancel (State : Task_State; C : Continuation'Class) is
   begin
      State.Object.Cancel(C);
   end Cancel;

end Task_States;
