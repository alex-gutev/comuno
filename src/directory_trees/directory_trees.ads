--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Iterator_Interfaces;

with Listers;
with File_System;
with Directory_Entries; use Directory_Entries;
with Paths;
with Paths.Canonical_Paths;

--
-- Purpose:
--
--  Provides an abstract Directory_Tree type, with associated
--  operations, for organizing a list of files, returned by a Lister
--  object, into a tree of directories.
--
-- Usage:
--
--  1. Create Directory_Tree.
--  2. Add entries to the tree using Add_Entry.
--  3. Retrieve the elements using Iterate.
--
package Directory_Trees is
   --
   -- Directory_Tree
   --
   --  Stores a set of entries as a tree of directories.
   --
   type Directory_Tree is abstract tagged private;


   -- Exceptions --

   --
   -- Entry_Not_In_Tree
   --
   --  Exception to be raised when an entry is requested which is not
   --  in the tree.
   --
   Entry_Not_In_Tree : exception;

   --
   -- Not_Subpath
   --
   --  Exception to be raised when attempting to set the subpath to a
   --  path which does not correspond to a subdirectory in the tree
   --
   Not_Subdirectory : exception;


   -- Operations --

   --
   -- Add_Entry
   --
   --  Add a new entry to the tree.
   --
   procedure Add_Entry (This : in out Directory_Tree; New_Entry : in Directory_Entry)
     is abstract;


   --
   -- Subpath
   --
   --  Returns the path of the current subdirectory of the tree.
   --
   --  The subpath controls which entries are visited by the Iterate
   --  procedure.
   --
   function Subpath (This : Directory_Tree) return Paths.Canonical_Paths.Canonical_Path
     is abstract;

   --
   -- Set_Subpath
   --
   --  Set the tree's current subdirectory.
   --
   procedure Set_Subpath (This : in out Directory_Tree;
                          Path : in Paths.Canonical_Paths.Canonical_Path)
     is abstract;


   --
   -- Is_Subdir
   --
   --  Returns true if an entry (Ent) designates a subdirectory in the
   --  tree.
   --
   function Is_Subdir (This : Directory_Tree; Ent : Directory_Entry) return Boolean
     is abstract;

   --
   -- At_Basedir
   --
   --  Returns true if the current subdirectory of the tree is the
   --  tree's base directory.
   --
   function At_Basedir (This : Directory_Tree) return Boolean
     is abstract;


   --
   -- Has_Entry
   --
   --  Returns true if the tree has an entry at the subpath Name.
   --
   function Has_Entry (This : Directory_Tree; Name : Paths.Path) return Boolean
     is abstract;

   --
   -- Get_Entry
   --
   --  Retrieve the entry at the subpath Name.
   --
   -- Exceptions:
   --
   --  If the tree does not contain an entry at subpath Name, an
   --  Entry_Not_In_Tree exception is raised.
   --
   function Get_Entry (This : Directory_Tree; Name : Paths.Path) return Directory_Entry
     is abstract;


   --
   -- Iterate
   --
   --  Call the procedure F, of one argument, on each entry in a
   --  subdirectory of the tree.
   --
   procedure Iterate (This    : in     Directory_Tree;
                      Subpath : in     Paths.Canonical_Paths.Canonical_Path;
                      F       : access procedure (E : Directory_Entry))
     is abstract;

private

   type Directory_Tree is abstract tagged null record;

end Directory_Trees;
