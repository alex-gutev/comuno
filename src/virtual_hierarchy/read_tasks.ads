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
with Ada.Exceptions;

with Paths;
with Directory_Entries;
with Directory_Trees;
with Directory_Types;

with Task_States;
with Task_Cleanup;

with Background_Tasks;

--
-- Purpose:
--
--  Provides a task for reading a directory in the background.
--
generic
   --
   -- Type of the user data object passed to each callback.
   --
   type User_Data is private;

   --
   -- Callback, which is called before reading the directory.
   --
   with procedure Begin_Callback (Data : in User_Data);

   --
   -- Callback, which is called when reading an entry.
   --
   with procedure Entry_Callback (Data : in User_Data; Dir_Entry : in Directory_Entries.Directory_Entry);

   --
   -- Callback, which is called after the task has finished reading
   -- the directory.
   --
   -- The callback should call the Output entry of the task, from a
   -- different task than the one on which it is invoked, to allow the
   -- task to terminate.
   --
   with procedure Finish_Callback (Data           : in User_Data;
                                   Directory_Type : in Directory_Types.Directory_Type'Class);

   --
   -- Callback, which is called when an error occurs.
   --
   with procedure Error_Callback (Data : in User_Data; Error : in Ada.Exceptions.Exception_Occurrence);

package Read_Tasks is

   -- Indefinite Holder Packages --

   package Tree_Holders renames Background_Tasks.Tree_Holders;


   -- Read Task --

   --
   -- Read_Task
   --
   --  Task for reading a directory.
   --
   task type Read_Task is new Background_Tasks.Background_Task and
     Task_Cleanup.Tracked_Task with

      --
      -- Init
      --
      --  Initialize the task with a Task_State object, for
      --  cancellation, and a callback user data object.
      --
      entry Init (Task_State : Task_States.Task_State_Ref;
                  Data : User_Data);

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
      -- Output
      --
      --  Sets Directory_Tree to the directory tree object into which
      --  the entries were read.
      --
      entry Output (Directory_Tree : in out Tree_Holders.Holder);

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
