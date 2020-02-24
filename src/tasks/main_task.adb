--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Glib.Main;

package body Main_Task is

   --
   -- User data object passed to the idle function.
   --
   type Idle_Data is record
      Proc : Dispatch_Procedure; -- Procedure to execute
      Data : Data_Type;          -- Procedure's user data
   end record;

   package Idle_Source is new Glib.Main.Generic_Sources (Idle_Data);

   function Idle_Func (Data : Idle_Data) return Boolean is
   begin
      -- Execute Procedure
      Data.Proc.all(Data.Data);

      -- Return false to remove the function from the list of idle
      -- callbacks.
      return False;
   end Idle_Func;


   procedure Dispatch (Proc : Dispatch_Procedure; Data : Data_Type) is
      Dummy : Glib.Main.G_Source_Id;

   begin
      Dummy := Idle_Source.Idle_Add(Idle_Func'Access, (Proc => Proc, Data => Data));
   end Dispatch;


end Main_Task;
