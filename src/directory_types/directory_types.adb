--  Copyright (C) 2020 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

with File_System;
with Regular_Directory_Types;

with Paths;
with Paths.Canonical_Paths;

with Listers;
with Directory_Listers;

package body Directory_Types is

   subtype Canonical_Path is Paths.Canonical_Paths.Canonical_Path;

   -- Utilities --

   --
   -- Canonicalize_Case
   --
   --  Convert the string representation of Path to its canonical
   --  case.
   --
   --  Dir is set to the portion of the Path, in its canonical case,
   --  which corresponds to an actual on disk directory. Subpath is
   --  set to the remainder of Path, which does not correspond to an
   --  on disk directory, i.e. the directory at that path could not be
   --  opened.
   --
   procedure Canonicalize_Case
     (Path    : in  Canonical_Path;
      Dir     : out Paths.Path;
      Subpath : out Paths.Path);

   --
   -- Find_Matching_Component
   --
   --  Find an entry, in the directory at path Directory, of which the
   --  name is either equal to Component or is case insensitive equal
   --  to component.
   --
   --  If Component is the file system root it is returned directly.
   --
   function Find_Matching_Component
     (Directory : Paths.Path;
      Component : Paths.Unbounded_Path_String)
     return Paths.Unbounded_Path_String;


   -- Public Functions --

   function Get_Type (Path : Paths.Path) return Directory_Type'Class is
      Dir     : Paths.Path;
      Subpath : Paths.Path;

   begin
      Canonicalize_Case
        (Paths.Canonical_Paths.Canonicalize
           (Paths.Expand_Tilde(Path)),
         Dir,
         Subpath);

      -- TODO: If Subpath is not empty create an archive lister.

      return Regular_Directory_Types.Create(Dir);
   end Get_Type;

   function Get_Type (Path : Paths.Path; File : Directory_Entries.Directory_Entry) return Directory_Type'Class is
      use Directory_Entries;

      File_Path : Paths.Path renames Paths.Path(Subpath(File));

   begin

      case Kind(File) is
         when File_System.Directory =>
            return Regular_Directory_Types.Create(Path.Append(File_Path));

         when others =>
            raise Not_Directory;
      end case;

   end Get_Type;


   -- Case Canonicalization --

   procedure Canonicalize_Case
     (Path    : in  Canonical_Path;
      Dir     : out Paths.Path;
      Subpath : out Paths.Path) is

      Components : Paths.Component_Array := Path.Components;

   begin

      Dir := Paths.Make_Path("");
      Subpath := Paths.Make_Path("");

      for I in Components'Range loop
         begin
            Dir.Append
              (Paths.Make_Path
                 (Find_Matching_Component(Dir, Components(I))));

         exception
            when Listers.Open_Directory_Error =>
               Subpath := Paths.Make_Path(Components(I .. Components'Last), Path.Is_Directory);
               return;

         end;
      end loop;

   end Canonicalize_Case;

   function Find_Matching_Component
     (Directory : Paths.Path;
      Component : Paths.Unbounded_Path_String)
     return Paths.Unbounded_Path_String is

      use Ada.Strings.Unbounded;

      function Equals (S1, S2 : Unbounded_String) return Boolean renames
        Ada.Strings.Unbounded.Equal_Case_Insensitive;

   begin
      if Component = Paths.Separator_String then
         return Component;
      end if;

   Search_Dir:
      declare
         Lister : Directory_Listers.Directory_Lister;
         Ent    : Listers.Dir_Entry;

         Match  : Unbounded_String;

      begin
         Lister.Open(Directory.To_String);

         while Lister.Read_Entry(Ent) loop
            if Ent.Name = Component then
               return Component;

            elsif Match = Null_Unbounded_String and
              Equals(Component, Ent.Name) then
               Match := Ent.Name;

            end if;
         end loop;

         return (if Match /= Null_Unbounded_String then
                    Match
                 else Component);

      exception
         when Listers.Read_Entry_Error =>
            return (if Match /= Null_Unbounded_String then
                       Match
                    else Component);

      end Search_Dir;
   end Find_Matching_Component;

end Directory_Types;
