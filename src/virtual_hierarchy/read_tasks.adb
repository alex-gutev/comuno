--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with File_System;
with Listers;

with Ada.Exceptions;

package body Read_Tasks is
   use type Directory_Types.Directory_Type;

   -- Holder Packages

   package Type_Holders is new Ada.Containers.Indefinite_Holders
     (Directory_Types.Directory_Type'Class);


   -- Utility Functions --

   procedure Call_Begin (State : in out Task_States.Background_State;
                         Data : in User_Data) is
   begin
      State.Enter_Foreground;
      Begin_Callback(Data);
      State.Exit_Foreground;
   end Call_Begin;

   procedure Add_Entry (Lister : in out Listers.Lister'Class;
                        Tree : in out Directory_Trees.Directory_Tree'Class;
                        Dir_Entry : in Listers.Dir_Entry) is

      Attributes : File_System.Attributes;

   begin
      Attributes := Lister.Entry_Attributes;
      Tree.Add_Entry(Directory_Entries.Make_Entry(Dir_Entry, Attributes));

   exception
      when Listers.Get_Attributes_Error => null;

   end Add_Entry;


   -- Read Task

   task body Read_Task is
      State : Task_States.Background_State;
      Cb_Data : User_Data;

      Dir_Type : Type_Holders.Holder;
      Tree : Tree_Holders.Holder;

      procedure Call_New_Entry (Ent : in Directory_Entries.Directory_Entry) is
      begin
         State.Enter_Foreground;
         Entry_Callback(Cb_Data, Ent);
         State.Exit_Foreground;
      end Call_New_Entry;

   begin
      accept Init (Task_State : Task_States.Task_State_Ref;
                   Data : User_Data) do

         State := Task_State.Get_Background_State;
         Cb_Data := Data;
      end Init;

      declare
         Dir_Path : Paths.Path;

      begin
         select
            accept Read (Path : Paths.Path) do
               Dir_Path := Path;
            end Read;

            Dir_Type.Replace_Element(Directory_Types.Get_Type(Dir_Path));

         or
            accept Read (Directory_Type : Directory_Types.Directory_Type'Class)  do
               Dir_Type.Replace_Element(Directory_Type);
            end Read;

         end select;
      end;

      Call_Begin(State, Cb_Data);

      Tree.Replace_Element(Dir_Type.Reference.Make_Tree);


      declare
         Lister : Listers.Lister'Class := Dir_Type.Reference.Make_Lister;
         Ent    : Listers.Dir_Entry;

      begin

         while Lister.Read_Entry(Ent) loop
            Add_Entry(Lister, Tree.Reference, Ent);
         end loop;

      end;

      Tree.Reference.Iterate(Call_New_Entry'Access);

      State.Enter_Foreground;

      Finish_Callback(Cb_Data, Dir_Type.Reference);

      accept Finish (Directory_Tree : in out Tree_Holders.Holder) do
         Directory_Tree.Move(Tree);
      end Finish;

      State.Exit_Foreground;

   end Read_Task;

   function Create return Read_Task_Ptr is
      T : Read_Task_Ptr := new Read_Task;

   begin
      Task_Cleanup.Track(Task_Cleanup.Tracked_Task_Ptr(T));
      return T;
   end Create;


end Read_Tasks;
