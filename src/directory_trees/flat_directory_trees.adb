--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Flat_Directory_Trees is

   -- Adding Entries --

   procedure Add_Entry (This : in out Flat_Directory_Tree; Ent : in Directory_Entry) is
   begin
      This.Entries.Include(Paths.Path(Subpath(Ent)), Ent);
   end Add_Entry;


   -- Subdirectories --

   function Subpath (This : in Flat_Directory_Tree) return Paths.Path is
     (Paths.Make_Path(""));

   procedure Set_Subpath (This : in out Flat_Directory_Tree; Path : in Paths.Path) is
      pragma Unreferenced (Path);

      pragma Assertion_Policy (Check);
      pragma Assert(False, "Cannot set subpath of Flat_Directory_Tree");

   begin
      null;
   end Set_Subpath;


   function Is_Subdir (This : in Flat_Directory_Tree; Ent : in Directory_Entry) return Boolean is
     (False);

   function At_Basedir (This : in Flat_Directory_Tree) return Boolean is
     (True);


   -- Retrieving Entries --

   function Has_Entry (This : Flat_Directory_Tree; Name : Paths.Path) return Boolean is
      (This.Entries.Contains(Name));

   function Get_Entry (This : Flat_Directory_Tree; Name : Paths.Path) return Directory_Entry is
      Pos : File_Maps.Cursor := This.Entries.Find(Name);

   begin
      if File_Maps.Has_Element(Pos) then
         return File_Maps.Element(Pos);

      else
         raise Directory_Trees.Entry_Not_In_Tree;

      end if;
   end Get_Entry;


   procedure Iterate (This : Flat_Directory_Tree; F : access procedure (E : Directory_Entry)) is
   begin
      for Ent of This.Entries loop
         F.all(Ent);
      end loop;
   end Iterate;

end Flat_Directory_Trees;
