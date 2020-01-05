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
with Paths.Canonical_Paths;
with Directory_Entries; use Directory_Entries;
with Directory_Trees;

private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Hashed_Sets;

--
-- Purpose
--
--  Provides an implementation of the Directory_Tree abstract type,
--  which stores entries as a tree of directories. The directory
--  information is obtained from the components of the paths of the
--  entries added to the tree.
--
--  This Directory_Tree can store multiple entries with the same path,
--  however only a single directory entry, per subdirectory is stored.
--
package Hierarchical_Directory_Trees is

   type Directory_Tree is new Directory_Trees.Directory_Tree with private;


   -- Operations --

   procedure Add_Entry (This : in out Directory_Tree; New_Entry : in Directory_Entry);


   function Subpath (This : Directory_Tree) return Paths.Path;

   procedure Set_Subpath (This : in out Directory_Tree; Path : in Paths.Path);


   function Is_Subdir (This : Directory_Tree; Ent : Directory_Entry) return Boolean;

   function At_Basedir (This : Directory_Tree) return Boolean;


   function Has_Entry (This : Directory_Tree; Name : Paths.Path) return Boolean;

   function Get_Entry (This : Directory_Tree; Name : Paths.Path) return Directory_Entry;


   procedure Iterate (This : in Directory_Tree; F : access procedure (E : Directory_Entry));

private

   --
   -- File_Key
   --
   --  Map key identifying an entry.
   --
   --  Since there may be multiple entries with the same key, each
   --  entry has an index associated with it.
   --
   type File_Key is record
      Path  : Paths.Canonical_Paths.Canonical_Path;
      Index : Positive;
   end record;

   function Hash (Key : File_Key) return Ada.Containers.Hash_Type;

   function "=" (Key1, Key2 : File_Key) return Boolean;


   package File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => File_Key,
      Element_Type    => Directory_Entry,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => File_Key,
      Hash => Hash,
      Equivalent_Elements => "=");

   package Directory_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Paths.Canonical_Paths.Canonical_Path,
      Element_Type    => File_Sets.Set,
      Hash            => Paths.Canonical_Paths.Hash,
      Equivalent_Keys => Paths.Canonical_Paths."=",
      "=" => File_Sets."=");


   type Directory_Tree is new Directory_Trees.Directory_Tree with record
      Entries     : File_Maps.Map;                        -- Map of all entries in the tree
      Directories : Directory_Maps.Map;                   -- Map storing the set of files in each subdirectory
      Subdir      : Paths.Canonical_Paths.Canonical_Path; -- Tree subdirectory
   end record;

end Hierarchical_Directory_Trees;
