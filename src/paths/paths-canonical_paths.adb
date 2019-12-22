--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Paths.String_Paths;

package body Paths.Canonical_Paths is
   use type Ada.Containers.Count_Type;
   use type Component_Vectors.Vector;


   -- Constructors --

   function Make_Path (Components : Component_Vector; Directory : Boolean) return Canonicalized_Path is
     (Canonicalized_Path'(Path_Components => Components, Directory => Directory));


   -- Conversions --

   function To_String (P : Canonicalized_Path) return Path_String is
      New_Path : String_Paths.String_Path :=
        String_Paths.Make_Path(P.Path_Components, P.Directory);

   begin
      return String_Paths.To_String(New_Path);
   end To_String;


   -- Path Components --

   function Components (P : Canonicalized_Path) return Component_Vector is
     (P.Path_Components);

   function Basename (P : Canonicalized_Path) return Path_String is
   begin
      if not P.Directory then
         case P.Path_Components.Length is
            when 0 => return "";
            when 1 =>
               if not P.Is_Root then
                  return To_String(P.Path_Components.First_Element);
               end if;

            when others =>
               return To_String(P.Path_Components.Last_Element);
         end case;
      end if;

      return "";
   end Basename;


   -- Path Predicates --

   function Is_Directory (P : Canonicalized_Path) return Boolean is
     (P.Directory or P.Is_Root);

   function Has_Directories (P : Canonicalized_Path) return Boolean is
   begin
      if P.Path_Components.Length = 1 then
         declare
            First : Unbounded_Path_String := P.Path_Components.First_Element;
         begin
            return P.Directory or else
              First = Separator_String or else
              First = Self_Component or else
              First = Parent_Component or else
              (Length(First) > 0 and then Element(First, 1) = '~');
         end;

      elsif not P.Path_Components.Is_Empty then
         return True;

      else
         return False;

      end if;
   end Has_Directories;


   function Is_Empty (P : Canonicalized_Path) return Boolean is
     (P.Path_Components.Is_Empty);

   function Is_Root (P : Canonicalized_Path) return Boolean is
     (P.Path_Components.Length = 1 and then
        P.Path_Components.First_Element = Separator_String);

   function Is_Relative (P : Canonicalized_Path) return Boolean is
      function Is_Tilde (Component : Unbounded_Path_String) return Boolean is
        (Length(Component) > 0 and then Element(Component, 1) = '~');

   begin
      return P.Path_Components.Is_Empty or else
        (P.Path_Components.First_Element /= Separator_String and
         not Is_Tilde(P.Path_Components.First_Element));
   end Is_Relative;


   -- Operations on Paths --

   function Ensure_Directory (P : Canonicalized_Path; Directory : Boolean := True) return Canonicalized_Path is
      Copy : Canonicalized_Path := P;

   begin
      Copy.Ensure_Directory(Directory);
      return Copy;
   end Ensure_Directory;

   procedure Ensure_Directory (P : in out Canonicalized_Path; Directory : Boolean := True) is
   begin
      P.Directory := Directory;
   end Ensure_Directory;


   function Append (P1, P2 : Canonicalized_Path) return Canonicalized_Path is
      Copy : Canonicalized_Path := P1;

   begin
      Copy.Append(P2);
      return Copy;
   end Append;

   procedure Append (P1 : in out Canonicalized_Path; P2 : in Canonicalized_Path) is
      package Vec renames Component_Vectors;

      function Remove_Leading_Parents return Vec.Cursor is
      begin

         for C in P2.Path_Components.Iterate loop
            if P1.Path_Components.Is_Empty then
               return C;

            elsif Vec.Element(C) = ".." then
               P1.Path_Components.Delete_Last;

            else
               return C;

            end if;
         end loop;

         return Vec.No_Element;
      end Remove_Leading_Parents;

      Start : Vec.Cursor := Remove_Leading_Parents;

   begin

      if Vec.Has_Element(Start) then
         for Comp_Cursor in P2.Path_Components.Iterate(Start) loop
            P1.Path_Components.Append(Vec.Element(Comp_Cursor));
         end loop;

         P1.Directory := P2.Directory;

      else
         P1.Directory := True;

      end if;

   end Append;


   function Remove_Last_Component (P : Canonicalized_Path) return Canonicalized_Path is
      Copy : Canonicalized_Path := P;

   begin
      Copy.Remove_Last_Component;
      return Copy;
   end Remove_Last_Component;

   procedure Remove_Last_Component (P : in out Canonicalized_Path) is
   begin
      if not P.Path_Components.Is_Empty then
         P.Path_Components.Delete_Last;
      end if;
   end Remove_Last_Component;


   function Merge (P1, P2 : Canonicalized_Path) return Canonicalized_Path is
      Copy : Canonicalized_Path := P1;

   begin
      Copy.Merge(P2);
      return Copy;
   end Merge;

   procedure Merge (P1 : in out Canonicalized_Path; P2 : in Canonicalized_Path) is
   begin

      if P2.Is_Relative then
         if not P1.Is_Directory then
            P1.Remove_Last_Component;
         end if;

         P1.Append(P2);

      else
         P1.Path_Components := P2.Path_Components;
         P1.Directory := P2.Directory;

      end if;

   end Merge;


   function Canonicalize (P : Canonicalized_Path; Directory : Boolean := False) return Canonical_Path is
      Copy : Canonicalized_Path := P;

   begin
      Copy.Directory := Directory;
      return Copy;
   end Canonicalize;


   -- Comparing Paths --

   function "=" (P1 : Canonicalized_Path; P2 : Canonicalized_Path) return Boolean is
     (P1.Is_Directory = P2.Is_Directory and then P1.Path_Components = P2.Path_Components);

   function Is_Subpath (Child : Canonicalized_Path; Parent : Canonicalized_Path; Check_Directory : Boolean := False) return Boolean is
      package Vec renames Component_Vectors;

      function Test_Components return Boolean is
         Child_Cursor  : Vec.Cursor := Child.Path_Components.First;
         Parent_Cursor : Vec.Cursor := Parent.Path_Components.First;

      begin

         while Vec.Has_Element(Child_Cursor) and
           Vec.Has_Element(Parent_Cursor) loop

            if Vec.Element(Child_Cursor) /= Vec.Element(Parent_Cursor) then
               return False;
            end if;

            Vec.Next(Parent_Cursor);
            Vec.Next(Child_Cursor);
         end loop;

         return not Vec.Has_Element(Parent_Cursor);

      end Test_Components;

   begin
      if Parent.Path_Components.Length >= Child.Path_Components.Length then
         return False;

      elsif Parent.Path_Components.Length = 0 then
         return True;

      elsif Check_Directory and not Parent.Is_Directory then
         return False;

      else
         return Test_Components;

      end if;
   end Is_Subpath;


   function Is_Child (Child : Canonicalized_Path; Parent : Canonicalized_Path) return Boolean is
      package Vec renames Component_Vectors;

      Child_Cursor  : Vec.Cursor := Child.Path_Components.First;
      Parent_Cursor : Vec.Cursor := Parent.Path_Components.First;

   begin

      while Vec.Has_Element(Child_Cursor) and
        Vec.Has_Element(Parent_Cursor) loop

         if Vec.Element(Child_Cursor) /= Vec.Element(Parent_Cursor) then
            return False;
         end if;

         Vec.Next(Parent_Cursor);
         Vec.Next(Child_Cursor);
      end loop;

      return not Vec.Has_Element(Parent_Cursor) and
        Vec.Has_Element(Child_Cursor) and
        Vec.To_Index(Child_Cursor) = Vec.Extended_Index(Child.Path_Components.Length);

   end Is_Child;

end Paths.Canonical_Paths;
