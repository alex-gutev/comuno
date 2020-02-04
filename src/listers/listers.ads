--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with File_System; use File_System;
with Paths;

--
-- Purpose:
--
--  This package provides an interface for a listing the contents of a
--  directory, which may either be a regular on disk directory or some
--  other form of directory (such as an archive).
--
-- Usage:
--
--  - Create a Lister object to obtain a 'handle' to the directory to
--    be listed.
--
--  - Call Read_Entry to obtain the name and type of the next entry in
--    the directory.
--
--  - Call Entry_Attributes to obtain the attributes of the last entry
--    that was read.
--
package Listers is
   --
   -- Abstract type which serves as a handle to a directory.
   --
   --  Each directory lister should create a derived type of Lister,
   --  in which to store its state.
   --
   --  The lister object should release all file system resources,
   --  held by the object, in its Finalize operation.
   --
   type Lister is abstract new Limited_Controlled with null record;
   type Lister_Ptr is access all Lister'Class;

   --
   -- Directory Entry Record.
   --
   --  Record in which the information about an entry is returned.
   --
   type Dir_Entry is record
      Name : Paths.Unbounded_Path_String;
      Kind : File_Type;
   end record;


   -- Abstract Lister Operations --

   --
   -- Read_Entry
   --
   --  Retrieve the metadata of the next entry in a directory.
   --
   --  The metadata should be returned in the Ent out parameter.
   --
   --  The procedure should return True if an entry was actually read,
   --  False if there are no more entries.
   --
   function Read_Entry (This : in out Lister; Ent : out Dir_Entry) return Boolean
     is abstract;

   --
   -- Entry_Attributes
   --
   --  Retrieve the full attributes of the last entry that was read by
   --  the lister.
   --
   function Entry_Attributes (This : in out Lister) return Attributes
     is abstract;


   -- Exceptions --

   -- The following exceptions should be raised for errors occuring
   -- while listing the contents of a directory.

   Open_Dir_Error       : exception; -- Error opening directory for
                                     -- reading.

   Get_Attributes_Error : exception; -- Error obtaining file
                                     -- attributes

end Listers;
