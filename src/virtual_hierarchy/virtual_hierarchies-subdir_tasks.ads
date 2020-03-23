--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Paths.Canonical_Paths;
with Task_Cleanup;
with Task_States;

--
-- Purpose:
--
--  Provides a task which lists the contents of a subdirectory within
--  a virtual hierarchy.
--
--  The listing of the subdirectory is performed in a separate task,
--  in order to keep the UI responsive in the case of a large
--  directory.
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
   with procedure Finish_Callback (Data : in User_Data);

package Virtual_Hierarchies.Subdir_Tasks is

   subtype Canonical_Path is Paths.Canonical_Paths.Canonical_Path;


   -- Read Task --

   --
   -- Read_Task
   --
   --  Task for listing the subdirectory.
   --
   task type Read_Task is new Task_Cleanup.Tracked_Task with

      --
      -- Init
      --
      --  Begin listing the subdirectory at Subpath.
      --
      entry Init (Task_State : Task_States.Task_State_Ref;
                  Hierarchy  : Virtual_Hierarchy;
                  Subpath    : Canonical_Path;
                  Data       : User_Data);

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

end Virtual_Hierarchies.Subdir_Tasks;
