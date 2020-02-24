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
--  Provides functionality for executing a procedure on the main task.
--
generic
   --
   -- Type of the user data passed to the procedure.
   --
   type Data_Type is private;

package Main_Task is

   --
   -- Procedure type
   --
   type Dispatch_Procedure is access procedure (Data : Data_Type);

   --
   -- Dispatch
   --
   --  Execute a procedure (Proc) on the main thread, with a user data
   --  object (Data) passed to it.
   --
   procedure Dispatch (Proc : Dispatch_Procedure; Data : Data_Type);

end Main_Task;
