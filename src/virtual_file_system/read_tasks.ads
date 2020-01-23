--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Indefinite_Holders;

with Paths;
with Directory_Entries;
with Directory_Trees;
with Directory_Types;

with Task_States;
with Task_Cleanup;

--
-- Purpose:
--
--  Provides a task for reading a directory.
--
package Read_Tasks is

   -- Indefinite Holder Packages --

   package Tree_Holders is new Ada.Containers.Indefinite_Holders
     (Directory_Trees.Directory_Tree'Class,
      Directory_Trees."=");


   -- Background Task Interface --

   --
   -- Background task interface type.
   --
   --  This provides one primitive operation, Finish, which should be
   --  called, from a foreground task, to allow the background task to
   --  terminate.
   --
   type Background_Task is task interface;
   type Background_Task_Ptr is access all Background_Task'Class;

   --
   -- Finish
   --
   --  Informs the background task that it may terminate. Additionally
   --  the Directory_Tree object, storing the entries read from the
   --  directories, is moved into the holder object Tree.
   --
   procedure Finish (T : in Background_Task; Tree : in out Tree_Holders.Holder)
     is abstract;


   -- Operation Callback Interface --

   --
   -- Operation_Callback
   --
   --  Task Callback interface.
   --
   --  The interface provides three primitive operations which are
   --  called:
   --
   --   - Prior to beginning the operation.
   --   - When a new entry is read.
   --   - After all entries are read.
   --
   type Operation_Callback is interface;

   --
   -- Begin_Operation
   --
   --  Called before the task has begun reading the directory.
   --
   procedure Begin_Operation (Callback : in out Operation_Callback)
     is abstract;

   --
   -- New_Entry
   --
   --  Called when a new entry, Dir_Entry, is read from the directory.
   --
   procedure New_Entry (Callback : in out Operation_Callback;
                        Dir_Entry : in Directory_Entries.Directory_Entry)
     is abstract;

   --
   -- Finish_Operation
   --
   --  Called after all entries have been read.
   --
   --  This procedure should invoke the Finish entry of the background
   --  task, from a different task, in order to obtain the full
   --  directory tree and it to allow the task to terminate.
   --
   procedure Finish_Operation (Callback : in out Operation_Callback;
                               Task_Ptr : in Background_Task_Ptr;
                               Cancelled : in Boolean)
     is abstract;


   -- Read Task --

   --
   -- Read_Task
   --
   --  Task for reading a directory.
   --
   task type Read_Task is new Background_Task and Task_Cleanup.Tracked_Task with

      --
      -- Init
      --
      --  Initialize the task with a Task_State object, for
      --  cancellation, and a callback object.
      --
      --  Additionally the pointer to the task should be passed in
      --  order for it to be passed to the callback.
      --
      entry Init (Task_State : Task_States.Task_State_Ref;
                  Task_Ptr : Background_Task_Ptr;
                  Callback : Operation_Callback'Class);

      --
      -- Read
      --
      --  Begin reading the directory at Path.
      --
      entry Read (Path : Paths.Path);

      --
      -- Read
      --
      --  Begin reading the directory with type Directory_Type.
      --
      entry Read (Directory_Type : Directory_Types.Directory_Type'Class);

      --
      -- Finish
      --
      --  Allow the task to terminate. The full Directory Tree object,
      --  containing all the entries read, is moved into the Holder
      --  object passed as an argument.
      --
      entry Finish (Directory_Tree : in out Tree_Holders.Holder);

   end Read_Task;

   type Read_Task_Ptr is access Read_Task;

   --
   -- Create
   --
   --  Create a new Read_Task.
   --
   --  The task is dynamically created and configured to be
   --  automatically deallocated when it terminates.
   --
   function Create return Read_Task_Ptr;

end Read_Tasks;
