--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with File_System;
with Paths;
with Paths.Canonical_Paths;
with Directory_Entries; use Directory_Entries;
with Directory_Trees;

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

--
-- Purpose
--
--  Provides an implementation of the Directory_Tree abstract type,
--  which simply stores a flat list of entries.
--
--  This package is intended to be used when storing entries which are
--  already organized into directories, such as the entries read from
--  the file system.
--
--  The Flat_Directory_Tree type does not support
--  subdirectories. Trying to set the subdirectory will result in an
--  error.
--
package Flat_Directory_Trees is

   type Flat_Directory_Tree is new Directory_Trees.Directory_Tree with private;


   -- Operations --

   procedure Add_Entry (This : in out Flat_Directory_Tree; Ent : in Directory_Entry);


   function Subpath (This : in Flat_Directory_Tree) return Paths.Canonical_Paths.Canonical_Path;

   procedure Set_Subpath (This : in out Flat_Directory_Tree;
                          Path : in Paths.Canonical_Paths.Canonical_Path);


   function Is_Subdir (This : in Flat_Directory_Tree; Ent : in Directory_Entry) return Boolean;

   function At_Basedir (This : in Flat_Directory_Tree) return Boolean;


   function Has_Entry (This : Flat_Directory_Tree; Name : Paths.Path) return Boolean;

   function Get_Entry (This : Flat_Directory_Tree; Name : Paths.Path) return Directory_Entry;


   procedure Iterate (This    : in     Flat_Directory_Tree;
                      Subpath : in     Paths.Canonical_Paths.Canonical_Path;
                      F       : access procedure (E : Directory_Entry));

private

   package File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Paths.Path,
      Element_Type    => Directory_Entry,
      Hash            => Paths.Hash,
      Equivalent_Keys => Paths."=");

   type Flat_Directory_Tree is new Directory_Trees.Directory_Tree with record
      Entries : File_Maps.Map;
   end record;

end Flat_Directory_Trees;
