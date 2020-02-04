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
with Listers;
with Directory_Trees;
with Directory_Entries;

--
-- Purpose:
--
--  This package provides an abstract interface for creating Lister
--  and Directory_Tree objects, pertaining to a particular directory
--  type.
--
package Directory_Types is

   --
   -- Directory_Type
   --
   --  Abstract Lister and Directory_Tree factory.
   --
   --  A derived type should be provided for each type of directory.
   --
   type Directory_Type is abstract tagged private;


   -- Lister and Directory_Tree Creation --

   --
   -- Make_Lister
   --
   --  Create a lister for the directory.
   --
   function Make_Lister (Dir : in Directory_Type) return Listers.Lister'Class
     is abstract;

   --
   -- Make_Tree
   --
   --  Create a Directory_Tree object for storing entries read from
   --  the directory.
   --
   function Make_Tree (Dir : in Directory_Type) return Directory_Trees.Directory_Tree'Class
     is abstract;


   -- Directory_Type Properties --

   --
   -- Is_Regular_Directory
   --
   --  Return true if the directory is a regular on disk directory,
   --  accessibly via the file system API.
   --
   function Is_Regular_Directory (Dir : in Directory_Type) return Boolean
     is abstract;


   -- Directory_Type Paths --

   --
   -- Path
   --
   --  Returns the path to the physical directory file.
   --
   function Path (Dir : in Directory_Type) return Paths.Path
     is abstract;

   --
   -- Logical_Path
   --
   --  Returns the logical path to the directory.
   --
   --  The logical path is the path to the physical directory file
   --  with the subpath, within the virtual hierarchy, concatenated to
   --  it.
   --
   function Logical_Path (Dir : in Directory_Type) return Paths.Path
     is abstract;


   --
   -- Change_Subpath
   --
   --  Create a new Directory_Type object for the same physical
   --  directory but with a different subpath within the virtual
   --  hierarchy.
   --
   function Change_Subpath (Dir : in Directory_Type; Path : in Paths.Path) return Directory_Type
     is abstract;


   -- Directory_Type Factories --

   --
   -- Not_Directory
   --
   --  Raised when trying to obtain a Directory_Type for a
   --  non-directory.
   --
   Not_Directory : exception;

   --
   -- Get_Type
   --
   --  Create a Directory_Type object for the directory at Path.
   --
   --  If it is known that the file at Path is not to be a directory,
   --  the Not_Directory exception is raised.
   --
   function Get_Type (Path : Paths.Path) return Directory_Type'Class;

   --
   -- Get_Type
   --
   --  Create a Directory_Type object for a given entry, File, with
   --  the directory at Path.
   --
   --  If the entry is not a directory, the Not_Directory exception is
   --  raised.
   --
   function Get_Type (Path : Paths.Path; File : Directory_Entries.Directory_Entry) return Directory_Type'Class;

   --
   -- Get_Type
   --
   --  Create a Directory_Type object for a given entry, File, located
   --  within the directory corresponding to the Directory_Type object
   --  Dir.
   --
   --  If the entry is not a directory, the Not_Directory exception is
   --  raised.
   --
   function Get_Type (Dir : Directory_Type; File : Directory_Entries.Directory_Entry) return Directory_Type'Class
     is abstract;

private

   type Directory_Type is abstract tagged null record;

end Directory_Types;
