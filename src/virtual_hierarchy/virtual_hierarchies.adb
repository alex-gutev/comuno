--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Read_Tasks;
with Main_Task;

with Exception_Holders;

package body Virtual_Hierarchies is

   -- Holder Packages --

   package Callback_Holders is new Ada.Containers.Indefinite_Holders
     (Operation_Callback'Class);


   -- Utilities --

   function Get_Data (Ref : in Virtual_Hierarchy) return Data_Ref
     is (Ref.Data.Get);


   -- Initialization --

   overriding procedure Initialize (Ref : in out Virtual_Hierarchy) is
      Data : Virtual_Hierarchy_Data;

   begin
      Ref.Data.Set(Data);
   end Initialize;


   -- Accessors --

   function Path (Ref : in Virtual_Hierarchy) return Paths.Path is
      Data : Data_Ref := Ref.Data.Get;

   begin
      return Data.Directory_Type.Constant_Reference.Logical_Path;
   end Path;


   -- Read Task --

   --
   -- Read_Task_Data
   --
   --  User data object passed to the callbacks of the directory read
   --  task.
   --
   type Read_Task_Data is record
      Data       : Data_Weak_Ptr;
      Dir_Type   : Type_Holders.Holder;

      Task_Ptr   : Background_Tasks.Background_Task_Ptr;
      Task_State : Task_States.Cancellation_State;

      Callback   : Callback_Holders.Holder;
      Error      : Exception_Holders.Holder;
   end record;


   procedure Begin_Read (Data : in Read_Task_Data);

   procedure Read_Entry (Data : in Read_Task_Data; Ent : in Directory_Entries.Directory_Entry);

   procedure Finish_Read (Data : in Read_Task_Data; Dir_Type : in Directory_Types.Directory_Type'Class);

   procedure Read_Error (Data : in Read_Task_Data; Error : in Ada.Exceptions.Exception_Occurrence);


   -- Read_Tasks package Instantiation --

   package Read_Task is new Read_Tasks
     (User_Data       => Read_Task_Data,
      Begin_Callback  => Begin_Read,
      Entry_Callback  => Read_Entry,
      Finish_Callback => Finish_Read,
      Error_Callback  => Read_Error);

   --
   -- Read_Task_Continuation
   --
   --  Cancellation continuation callback which initiates a new read
   --  task.
   --
   type Read_Task_Continuation is new Task_States.Continuation with record
      State    : Task_States.Task_State_Ref; -- Read task state
      Data     : Data_Weak_Ptr;              -- Weak pointer to the shared hierarchy
      Callback : Callback_Holders.Holder;    -- Operation callback object
      Path     : Paths.Path;                 -- Path to directory read
   end record;

   overriding procedure Continue (C : in Read_Task_Continuation; Cancelled : Boolean);


   -- Reading Directory Hierarchy --

   procedure Read (Ref : in out Virtual_Hierarchy; Path : in Paths.Path; Callback : in Operation_Callback'Class) is
      Data : Data_Ref := Ref.Data.Get;
      State : Task_States.Task_State_Ref := Task_States.Create;

      Continuation : Read_Task_Continuation :=
        (State    => State,
         Data     => Ref.Data.Weak,
         Path     => Path,
         Callback => Callback_Holders.To_Holder(Callback));

   begin

      if not Data.Task_State.Is_Empty then
         Data.Task_State.Cancel(Continuation);
         Data.Task_State := State.Get_Cancellation_State;

      else
         Data.Task_State := State.Get_Cancellation_State;
         Continuation.Continue(False);

      end if;

   end Read;

   procedure Continue (C : in Read_Task_Continuation; Cancelled : Boolean) is
      T : Read_Task.Read_Task_Ptr := Read_Task.Create;

   begin
      T.Init(C.State,
             (Data       => C.Data,
              Callback   => C.Callback,
              Task_Ptr   => Background_Tasks.Background_Task_Ptr(T),
              Task_State => C.State.Get_Cancellation_State,
              others     => <>));

      T.Read(C.Path);
   end Continue;


   -- Descending into Directories --

   --
   -- Read_Task_Continuation
   --
   --  Cancellation continuation callback which initiates a new read
   --  task.
   --
   type Descend_Continuation is new Task_States.Continuation with record
      State    : Task_States.Task_State_Ref; -- Read task state
      Data     : Data_Weak_Ptr;              -- Weak pointer to the shared hierarchy
      Callback : Callback_Holders.Holder;    -- Operation callback object
      Dir_type : Type_Holders.Holder;        -- Path to directory read
   end record;

   overriding procedure Continue (C : in Descend_Continuation; Cancelled : Boolean);

   function Descend (Ref       : in out Virtual_Hierarchy;
                     Dir_Entry : in Directory_Entries.Directory_Entry;
                     Callback  : in Operation_Callback'Class)
                    return Boolean is

      Data  : Data_Ref                   := Ref.Data.Get;

   begin
      if Data.Current_Tree.Reference.Is_Subdir(Dir_Entry) then
         -- TODO: Read subdirectory
         return True;

      else
         declare
            Dir_Type : Type_Holders.Holder :=
              Type_Holders.To_Holder
                (Data.Directory_Type.Reference.Get_Type(Dir_Entry));

            State : Task_States.Task_State_Ref := Task_States.Create;

            Continuation : Descend_Continuation :=
              (State    => State,
               Data     => Ref.Data.Weak,
               Dir_Type => Dir_Type,
               Callback => Callback_Holders.To_Holder(Callback));

         begin
            Data.Task_State.Cancel(Continuation);
            Data.Task_State := State.Get_Cancellation_State;
         end;

         return True;
      end if;

   exception
      when Directory_Types.Not_Directory =>
         return False;

   end Descend;

   procedure Continue (C : in Descend_Continuation; Cancelled : Boolean) is
      T : Read_Task.Read_Task_Ptr := Read_Task.Create;

   begin
      T.Init(C.State,
             (Data       => C.Data,
              Callback   => C.Callback,
              Task_Ptr   => Background_Tasks.Background_Task_Ptr(T),
              Task_State => C.State.Get_Cancellation_State,
              others     => <>));

      T.Read(C.Dir_Type.Constant_Reference);

   end Continue;


   -- Read Callbacks --

   procedure Begin_Read (Data : in Read_Task_Data) is
   begin
      Data.Callback.Constant_Reference.Begin_Operation;
   end Begin_Read;

   procedure Read_Entry (Data : in Read_Task_Data; Ent : in Directory_Entries.Directory_Entry) is
   begin
      Data.Callback.Constant_Reference.New_Entry(Ent);
   end Read_Entry;


   -- Finishing Read Task --

   package Finish_Main is new Main_Task (Read_Task_Data);

   procedure Finish_Read_On_Main (Data : in Read_Task_Data) is
      Ptr : Data_Ptr;

   begin
      Ptr.Set(Data.Data);

      if not Ptr.Is_Null then
         declare
            Ref : Data_Ref := Ptr.Get;

         begin
            Data.Task_Ptr.Output(Ref.Current_Tree);
            Ref.Directory_Type := Data.Dir_Type;

            Data.Task_State.Finish;

            Data.Callback.Constant_Reference.Finish_Operation;
         end;
      end if;

   end Finish_Read_On_Main;

   procedure Finish_Read (Data : in Read_Task_Data; Dir_Type : in Directory_Types.Directory_Type'Class) is
      New_Data : Read_Task_Data := Data;

   begin
      New_Data.Dir_Type.Replace_Element(Dir_Type);
      Finish_Main.Dispatch(Finish_Read_On_Main'Access, New_Data);
   end Finish_Read;


   -- Handling Errors while Reading

   procedure Read_Error_On_Main (Data : in Read_Task_Data) is
      Ptr : Data_Ptr;

   begin
      Ptr.Set(Data.Data);

      if not Ptr.Is_Null then
         declare
            Ref : Data_Ref := Ptr.Get;

         begin
            Data.Task_State.Finish;
            Data.Callback.Constant_Reference.Operation_Error(Data.Error.Get);
         end;
      end if;

   end Read_Error_On_Main;

   procedure Read_Error (Data : in Read_Task_Data; Error : in Ada.Exceptions.Exception_Occurrence) is
      New_Data : Read_Task_Data := Data;

   begin
      New_Data.Error.Set(Error);
      Finish_Main.Dispatch(Read_Error_On_Main'Access, New_Data);
   end Read_Error;

end Virtual_Hierarchies;
