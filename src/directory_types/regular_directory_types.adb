--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Directory_Listers;
with Flat_Directory_Trees;

package body Regular_Directory_Types is

   function Create (Path : Paths.Path) return Directory_Type is
     (Directory_Types.Directory_Type with Dir_Path => Path);

   function Make_Lister (This : in Directory_Type) return Listing.Lister'Class is
   begin
      return Lister : Directory_Listers.Directory_Lister do
         Lister.Open(This.Dir_Path.To_String);
      end return;
   end Make_Lister;

   function Make_Tree (This : in Directory_Type) return Directory_Trees.Directory_Tree'Class is
      Tree : Flat_Directory_Trees.Flat_Directory_Tree;
   begin
      return Tree;
   end Make_Tree;


   function Is_Regular_Directory (This : in Directory_Type) return Boolean is
     (True);

   function Path (This : in Directory_Type) return Paths.Path is
     (This.Dir_Path);

   function Logical_Path (This : in Directory_Type) return Paths.Path is
     (This.Dir_Path);


   function Change_Subpath (This : in Directory_Type; Path : in Paths.Path) return Directory_Type is
      pragma Unreferenced (Path);

      pragma Assertion_Policy (Check);
      pragma Assert(False, "Cannot change subpath of Regular_Directory_Type");

   begin
      return This;
   end Change_Subpath;


   function Get_Type (This : Directory_Type; File : Directory_Entries.Directory_Entry) return Directory_Types.Directory_Type'Class is
   begin
      return Directory_Types.Get_Type(This.Dir_Path, File);
   end Get_Type;

end Regular_Directory_Types;
