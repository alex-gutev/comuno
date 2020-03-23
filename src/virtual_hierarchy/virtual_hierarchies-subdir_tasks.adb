--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body Virtual_Hierarchies.Subdir_Tasks is

   task body Read_Task is
      State : Task_States.Background_State;
      Cancellation : Task_States.Cancellation_State;

      Vfs : Virtual_Hierarchy;

      Path : Canonical_Path;
      Cb : Callback_Holders.Holder;

      Cb_Data : User_Data;

      procedure Call_New_Entry (E : Directory_Entries.Directory_Entry) is
      begin
         Entry_Callback(Cb_Data, E);
      end Call_New_Entry;


   begin
      accept Init (Task_State : Task_States.Task_State_Ref;
                   Hierarchy  : Virtual_Hierarchy;
                   Subpath    : Canonical_Path;
                   Data       : User_Data) do

         State := Task_State.Get_Background_State;
         Cancellation := Task_State.Get_Cancellation_State;
         Vfs := Hierarchy;

         Path := Subpath;
         Cb_Data := Data;
      end Init;


      declare
         Data : Data_Ref := Vfs.Data.Get;

      begin

         -- Call Begin Callback --

         State.Enter_Foreground;
         Begin_Callback(Cb_Data);
         State.Exit_Foreground;


         -- Call New_Entry on Subdirectory Entries --

         State.Enter_Foreground;
         Data.Current_Tree.Reference.Iterate(Path, Call_New_Entry'Access);
         State.Exit_Foreground;


         -- Finish Task --

         State.Enter_Foreground;

         Finish_Callback(Cb_Data);

      end;

   end Read_Task;


   -- Task Creation --

   function Create return Read_Task_Ptr is
      T : Read_Task_Ptr := new Read_Task;

   begin
      Task_Cleanup.Track(Task_Cleanup.Tracked_Task_Ptr(T));
      return T;
   end Create;

end Virtual_Hierarchies.Subdir_Tasks;
