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
with Paths; use Paths;
with Listing; use Listing;

private with C_Types;

--
-- Purpose:
--
--  This package provides an implementation of the Lister interface
--  for regular directories.
--
package Directory_Listers is
   type Directory_Lister is new Lister with private;
   type Directory_Lister_Ptr is access all Directory_Lister'Class;

   -- Creation and Finalization --

   --
   -- Open
   --
   --  Open the directory at Path for reading.
   --
   procedure Open (This : out Directory_Lister; Path : in Paths.Path_String);

   procedure Finalize (This : in out Directory_Lister);


   -- Lister Operations --

   function Read_Entry (This : in out Directory_Lister; Ent : out Dir_Entry) return Boolean;
   function Entry_Attributes (This : in out Directory_Lister) return Attributes;

private
   type Directory_Lister is new Lister with
      record
         Handle         : C_Types.Handle_Ptr;
         Last_Entry     : Dir_Entry;
      end record;
end Directory_Listers;
