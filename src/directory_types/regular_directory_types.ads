--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Paths;
with Listing;
with Directory_Entries;
with Directory_Trees;
with Directory_Types;

--
-- Purpose:
--
--  Provides an implementation of the Directory_Type interface for
--  regular on disk directories.
--
package Regular_Directory_Types is

   type Directory_Type is new Directory_Types.Directory_Type with private;


   function Create (Path : Paths.Path) return Directory_Type;


   function Make_Lister (This : in Directory_Type) return Listing.Lister'Class;

   function Make_Tree (This : in Directory_Type) return Directory_Trees.Directory_Tree'Class;


   function Is_Regular_Directory (This : in Directory_Type) return Boolean;


   function Path (This : in Directory_Type) return Paths.Path;

   function Logical_Path (This : in Directory_Type) return Paths.Path;


   --
   -- Change_Subpath
   --
   --  This function should not be called as regular directories do
   --  not represent a virtual hierarchy.
   --
   function Change_Subpath (This : in Directory_Type; Path : in Paths.Path) return Directory_Type;

   function Get_Type (This : Directory_Type; File : Directory_Entries.Directory_Entry) return Directory_Types.Directory_Type'Class;

private

   type Directory_Type is new Directory_Types.Directory_Type with record
      Dir_Path : Paths.Path;
   end record;

end Regular_Directory_Types;
