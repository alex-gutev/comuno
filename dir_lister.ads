--  Copyright (C) 2019 Alexander Gutev
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with File_System; use File_System;
with Listing; use Listing;
with C_Types;

--
-- Purpose:
--
--  This package provides an implementation of the Lister interface
--  for regular directories.
--
package Dir_Lister is
   type Dir_Lister is new Lister with private;
   type Dir_Lister_Ptr is access all Dir_Lister'Class;

   -- Creation and Finalization --

   --
   -- Open
   --
   --  Open the directory at Path for reading.
   --
   procedure Open (This : out Dir_Lister; Path : in String);

   procedure Finalize (This : in out Dir_Lister);


   -- Lister Operations --

   function Read_Entry (This : in out Dir_Lister; Ent : out Dir_Entry) return Boolean;
   function Entry_Attributes (This : in out Dir_Lister) return Attributes;

private
   type Dir_Lister is new Lister with
      record
         Handle         : C_Types.Handle_Ptr;
         Last_Entry     : Dir_Entry;
      end record;
end Dir_Lister;
