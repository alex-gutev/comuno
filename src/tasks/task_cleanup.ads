--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

--
-- Purpose:
--
--  Automatically deallocates tasks when they terminate.
--
package Task_Cleanup is

   --
   -- Tracked_Task Interface
   --
   --  This interface does not declare any subprograms however serves
   --  as an abstract task type.
   --
   --  All tasks, which are to be automatically deallocated on
   --  termination, should derive from this interface.
   --
   type Tracked_Task is task interface;
   type Tracked_Task_Ptr is access all Tracked_Task'Class;

   --
   -- Track
   --
   --  Track a task so that it is automatically deallocated when it
   --  terminates.
   --
   procedure Track (Ptr : Tracked_Task_Ptr);

end Task_Cleanup;
