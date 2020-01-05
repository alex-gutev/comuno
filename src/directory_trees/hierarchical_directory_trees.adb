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

package body Hierarchical_Directory_Trees is

   package Fs renames File_System;
   package Canonical_Paths renames Paths.Canonical_Paths;

   use type Fs.File_Type;
   use type Canonical_Paths.Canonical_Path;

   subtype Path_Key is Paths.Canonical_Paths.Canonical_Path;

   function Canonicalize (Path : Paths.Path'Class; Directory : Boolean := False) return Canonical_Paths.Canonical_Path
     renames Canonical_Paths.Canonicalize;


   -- Utility Functions for Adding Entries --

   --
   -- Add_Directory_Entry
   --
   --  Add an entry to a tree, which represents a subdirectory, if the
   --  tree does not already contain such an entry.
   --
   --  Returns the map key of the directory entry.
   --
   function Add_Directory_Entry (This : in out Directory_Tree; Path : Path_Key) return File_Key is
      Dir_Path : Path_Key := Path.Ensure_Directory;
      Key      : File_Key := (Dir_Path, 1);

   begin
      if not This.Entries.Contains(Key) then
         This.Entries.Insert(Key, Make_Entry(Paths.Path(Dir_Path), Fs.Directory));
      end if;

      return Key;
   end Add_Directory_Entry;

   --
   -- Add_Components
   --
   --  Add directory entries to a tree for each intermediate directory
   --  component of a path.
   --
   procedure Add_Components (This : in out Directory_Tree; Key : in File_Key) is
      package Vectors renames Paths.Component_Vectors;

      use type Vectors.Cursor;

      Components : Paths.Component_Vector := Key.Path.Components;
      Cursor     : Vectors.Cursor         := Components.First;
      Last       : Vectors.Cursor         := Components.Last;
      Subpath    : Path_Key;

   begin

      while Vectors.Has_Element(Cursor) loop
         if not This.Directories.Contains(Subpath) then
            declare
               Empty : File_Sets.Set;
            begin
               This.Directories.Insert(Subpath, Empty);
            end;
         end if;

         declare
            Component : Paths.Unbounded_Path_String   := Components(Cursor);
            Dir_Map   : Directory_Maps.Reference_Type := This.Directories.Reference(Subpath);

         begin
            Subpath.Append(Canonicalize(Paths.Make_Path(Component)));

            if Cursor /= Last then
               Dir_Map.Include(This.Add_Directory_Entry(Subpath));

            else
               Dir_Map.Include(Key);

            end if;
         end;

         Vectors.Next(Cursor);
      end loop;

   end Add_Components;


   -- Operations: Adding Entries --

   procedure Add_Entry (This : in out Directory_Tree; New_Entry : in Directory_Entry) is
      Path : Path_Key := Subpath(New_Entry);

      function Add_Replace_Dir_Entry return File_Key is
         Dir_Path : Path_Key := Path.Ensure_Directory;
         Cursor : File_Maps.Cursor := This.Entries.Find((Dir_Path, 1));
         Empty : File_Sets.Set;

      begin

         if File_Maps.Has_Element(Cursor) then
            This.Entries(Cursor) := New_Entry;

         else
            This.Entries.Insert((Dir_Path, 1), New_Entry);

         end if;

         This.Directories.Include(Path, Empty);

         return (Dir_Path, 1);
      end Add_Replace_Dir_Entry;

      function Add_File_Entry return File_Key is
         Cursor : File_Maps.Cursor;
         Index  : Positive := 1;

      begin
         loop
            Cursor := This.Entries.Find((Path, Index));
            exit when not File_Maps.Has_Element(Cursor);

            Index := Index + 1;
         end loop;

         This.Entries.Insert((Path, Index), New_Entry);
         return (Path, Index);
      end Add_File_Entry;

      Key : File_Key := (if Kind(New_Entry) = Fs.Directory then
                            Add_Replace_Dir_Entry else
                            Add_File_Entry);

   begin
      This.Add_Components(Key);
   end Add_Entry;


   -- Tree Subpath --

   function Subpath (This : Directory_Tree) return Paths.Path is
      (Paths.Path(This.Subdir));


   procedure Set_Subpath (This : in out Directory_Tree; Path : in Paths.Path) is
      Subpath : Path_Key := Canonical_Paths.Canonicalize(Path);

   begin

      if This.Directories.Contains(Subpath) then
         This.Subdir := Subpath;
      else
         raise Directory_Trees.Not_Subdirectory;
      end if;

   end Set_Subpath;


   function Is_Subdir (This : Directory_Tree; Ent : Directory_Entry) return Boolean is
     (Kind(Ent) = Fs.Directory and then
        This.Directories.Contains(Subpath(Ent)));

   function At_Basedir (This : Directory_Tree) return Boolean is
     (This.Subdir.Is_Empty);


   -- Retrieving Entries --

   function Has_Entry (This : Directory_Tree; Name : Paths.Path) return Boolean is
      (This.Entries.Contains((Canonical_Paths.Canonicalize(Name, Name.Is_Directory), 1)));

   function Get_Entry (This : Directory_Tree; Name : Paths.Path) return Directory_Entry is
      Pos : File_Maps.Cursor :=
        This.Entries.Find((Canonical_Paths.Canonicalize(Name, Name.Is_Directory), 1));

   begin
      if File_Maps.Has_Element(Pos) then
         return File_Maps.Element(Pos);
      else
         raise Directory_Trees.Entry_Not_In_Tree;
      end if;
   end Get_Entry;


   procedure Iterate (This : Directory_Tree; F : access procedure (E : Directory_Entry)) is
      Map : Directory_Maps.Constant_Reference_Type := This.Directories(This.Subdir);

   begin
      for Key of Map loop
         F.all(This.Entries(Key));
      end loop;
   end Iterate;


   -- Key Hashing --

   function Hash (Key : File_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      Prime : constant := 2654435761;
   begin
      return (Key.Path.Hash * Prime) xor Ada.Containers.Hash_Type(Key.Index);
   end Hash;

   function "=" (Key1, Key2 : File_Key) return Boolean is
      (Key1.Path = Key2.Path and Key1.Index = Key2.Index);

end Hierarchical_Directory_Trees;
