--  Copyright (C) 2020 Alexander Gutev All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Indefinite_Holders;
with Ada.Task_Attributes;

with Exception_Holders;

--
-- Purpose:
--
--  Provides functions for handling errors.
--
package Error_Handling is

   --
   -- Error
   --
   --  Stores information about the occurrence of an error.
   --
   type Error is record
      Error : Exception_Holders.Holder; -- Error Exception
   end record;

   --
   -- Handler
   --
   --  Error handler interface.
   --
   type Handler is interface;

   --
   -- Handle
   --
   --  Called when an error occurs, with 'E' containing information
   --  about the error.
   --
   --  The function should handle the error either by:
   --
   --   - Fixing the cause of the error and returning normally, in
   --     order for the failing operation to be retried.
   --
   --   - Raising an exception which is caught higher up the call
   --     stack, than the failing operation.
   --
   --  If the error cannot be handled, the function should reraise the
   --  error exception.
   --
   procedure Handle (This : Handler; E : Error) is abstract;


   -- Holder Package --

   package Handler_Holders is new Ada.Containers.Indefinite_Holders
     (Handler'Class);


   --
   -- Task_Error_Handler
   --
   --  The global error handler. Each task has its own handler
   --  function.
   --
   package Task_Error_Handler is new Ada.Task_Attributes
     (Handler_Holders.Holder,
      Handler_Holders.Empty_Holder);


   --
   -- Try_Op
   --
   --  Run the operation, designated by the procedure 'Op', catching
   --  all exceptions.
   --
   --  The Handle operation is called on the task's current global
   --  error handler, stored in Task_Error_Handler, and the error
   --  exception.
   --
   procedure Try_Op (Op : access procedure);

end Error_Handling;
