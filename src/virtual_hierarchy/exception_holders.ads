--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization;
with Ada.Exceptions;

--
-- Purpose:
--
--  Provides a non-limited Holder for an
--  Ada.Exceptions.Exception_Occurrence object.
--
package Exception_Holders is

   --
   -- Holder
   --
   --  Exception_Occurrence holder type, supporting copying.
   --
   type Holder is new Ada.Finalization.Controlled with private;

   overriding procedure Adjust (This : in out Holder);
   overriding procedure Finalize (This : in out Holder);

   --
   -- Set
   --
   --  Set the Holder's Exception_Occurrence object.
   --
   procedure Set (This : in out Holder; Error : in Ada.Exceptions.Exception_Occurrence);

   --
   -- Get
   --
   --  Returns the Holder's Exception_Occurrence object.
   --
   function Get (This : Holder) return Ada.Exceptions.Exception_Occurrence;

private

   type Holder is new Ada.Finalization.Controlled with record
      Error : Ada.Exceptions.Exception_Occurrence_Access;
   end record;

end Exception_Holders;
