--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Paths.Canonical_Paths;

package body Paths.String_Paths is

   -- Constructors --

   function Make_Path (Name : Path_String) return String_Path is
     (String_Path'(Name => To_Unbounded_String(Name)));

   function Make_Path (Name : Unbounded_Path_String) return String_Path is
     (String_Path'(Name => Name));


   function Make_Path (Name : Path_String; Directory : Boolean) return String_Path is
      P : String_Path := Make_Path(Name);

   begin
      P.Ensure_Directory(Directory);
      return P;
   end Make_Path;


   function Make_Path (Name : Unbounded_Path_String; Directory : Boolean) return String_Path is
      P : String_Path := Make_Path(Name);

   begin
      P.Ensure_Directory(Directory);
      return P;
   end Make_Path;


   function Make_Path (Components : Component_Array; Directory : Boolean := False) return String_Path is
      P : String_Path;

   begin
      for I in Components'Range loop
         P.Append(Make_Path(Components(I)));
      end loop;

      P.Ensure_Directory(Directory);

      return P;
   end Make_Path;

   function Make_Path (Components : Component_Vector; Directory : Boolean := False) return String_Path is
      P : String_Path;

   begin
      for Component of Components loop
         P.Append(Make_Path(Component));
      end loop;

      Ensure_Directory(P, Directory);

      return P;
   end Make_Path;


   -- Conversions --

   function To_String (P : String_Path) return Path_String is
     (To_String(P.Name));


   -- Path Components --

   function Components (P : String_Path) return Component_Vector is
      Comps : Component_Vector;
      Pos   : Positive := 1;
      Next  : Natural;

   begin
      if Length(P.Name) > 0 then
         loop
            Next := Index(P.Name, Separator_String, Pos);

            case Next is
               when 0 =>
                  Comps.Append(Unbounded_Slice(P.Name, Pos, Length(P.Name)));

               when 1 =>
                  Comps.Append(To_Unbounded_String(Separator_String));

               when others =>
                  if Next - Pos > 0 then
                     Comps.Append(Unbounded_Slice(P.Name, Pos, Next - 1));
                  end if;
            end case;

            exit when Next = 0 or Next = Length(P.Name);
            Pos := Next + 1;
         end loop;
      end if;

      return Comps;
   end Components;

   function Basename (P : String_Path) return Path_String is
   begin
      if Length(P.Name) > 0 then

         declare
            Last_Separator : Natural := Index(P.Name, Separator_String, Going => Backward);

         begin
            if Last_Separator = 0 then
               return To_String(P.Name);
            else
               return Slice(P.Name, Positive'(1 + Last_Separator), Length(P.Name));
            end if;
         end;
      end if;

      return To_String(P.Name);
   end Basename;


   -- Path Predicates --

   function Is_Directory (P : String_Path) return Boolean is
     (Length(P.Name) > 0 and then
        Element(P.Name, Length(P.Name)) = Separator);

   function Has_Directories (P : String_Path) return Boolean is
     (Index(P.Name, Separator_String) > 0 or else
        P.Name = Self_Component or else P.Name = Parent_Component or else
        (Length(P.Name) > 0 and then Element(P.Name, 1) = '~'));

   function Is_Empty (P : String_Path) return Boolean is
     (Length(P.Name) = 0);

   function Is_Root (P : String_Path) return Boolean is
      function Is_Root_Equivalent return Boolean is
      begin
         if Length(P.Name) > 0 then
            for I in 1 .. Length(P.Name) loop
               if Element(P.Name, I) /= Separator then
                  return False;
               end if;
            end loop;

            return True;

         else
            return False;
         end if;
      end Is_Root_Equivalent;


   begin
      return P.Name = Separator_String or else Is_Root_Equivalent;
   end Is_Root;

   function Is_Relative (P : String_Path) return Boolean is
     (Length(P.Name) = 0 or else
        (Element(P.Name, 1) /= Separator and Element(P.Name, 1) /= '~'));


   -- Operations on Paths --

   function Ensure_Directory (P : String_Path; Directory : Boolean := True) return String_Path is
      Copy : String_Path := P;
   begin
      Copy.Ensure_Directory(Directory);
      return Copy;
   end Ensure_Directory;

   procedure Ensure_Directory (P : in out String_Path; Directory : in Boolean := True) is
      Size : constant Natural := Length(P.Name);

   begin
      if Size > 0 then
         if Directory and Element(P.Name, Size) /= Separator then
            Append(P.Name, Separator);

         elsif not Directory and Element(P.Name, Size) = Separator and Size > 1 then
            Head(P.Name, Size - 1);

         end if;
      end if;
   end Ensure_Directory;


   function Append (P1 : String_Path; P2: String_Path) return String_Path is
      Copy : String_Path := P1;

   begin
      Copy.Append(P2);
      return Copy;
   end Append;

   procedure Append (P1 : in out String_Path; P2 : in String_Path) is
   begin
      P1.Ensure_Directory(True);

      Append(P1.Name, P2.Name);
   end Append;


   function Remove_Last_Component (P : String_Path) return String_path is
      Copy : String_Path := P;

   begin
      Copy.Remove_Last_Component;
      return Copy;
   end Remove_Last_Component;

   procedure Remove_Last_Component (P : in out String_Path) is
   begin
      if Length(P.Name) > 1 then
         declare
            Dir : Boolean  := P.Is_Directory;
            Max : Positive := Length(P.Name) - (if Dir then 1 else 0);
            Pos : Natural  := Index(P.Name, Separator_String, From => Max, Going => Backward);
         begin
            Head(P.Name, (if Pos > 1 and not Dir then Pos - 1 else Pos));
         end;
      else
         P.Name := Null_Unbounded_String;
      end if;
   end Remove_Last_Component;


   function Merge (P1 : String_Path; P2 : String_Path) return String_Path is
      Copy : String_Path := P1;

   begin
      Copy.Merge(P2);
      return Copy;
   end Merge;

   procedure Merge (P1 : in out String_Path; P2 : in String_Path) is
   begin
      if P2.Is_Relative then
         if not P1.Is_Directory then
            P1.Remove_Last_Component;
         end if;

         P1.Append(P2);
      else
         P1.Name := P2.Name;
      end if;
   end Merge;

   function Canonicalize (P : String_Path; Directory : Boolean := False) return Canonical_Path is
      Old_Components : Component_Vector := P.Components;
      New_Components : Component_Vector;

   begin
      for Component of Old_Components loop
         if Component = ".." then
            -- If at least one component and last component is not
            -- '..'
            if not New_Components.Is_Empty and then
              New_Components.Last_Element /= Parent_Component

            then
               New_Components.Delete_Last;

            else
               New_Components.Append(Component);

            end if;

         elsif Component /= Self_Component and Length(Component) > 0 then
            New_Components.Append(Component);

         end if;
      end loop;

      return Canonical_Paths.Make_Path(New_Components, Directory);
   end Canonicalize;


   -- Comparing Paths

   function "=" (P1 : String_Path; P2 : String_Path)  return Boolean is
     (P1.Name = P2.Name);

   function Is_Subpath (Child : String_Path; Parent : String_Path; Check_Directory : Boolean := False) return Boolean is
   begin
      if Length(Parent.Name) >= Length(Child.Name) then
         return False;

      elsif Length(Parent.Name) = 0 then
         return True;

      elsif Check_Directory and not Parent.Is_Directory then
         return False;

      elsif Head(Child.Name, Length(Parent.Name)) = Parent.Name then
         return Parent.Is_Directory or
           Element(Child.Name, Length(Parent.Name) + 1) = Separator;

      else
         return False;

      end if;
   end Is_Subpath;

   function Is_Child (Child : String_Path; Parent : String_Path) return Boolean is
      Child_Parent : String_Path := Child.Remove_Last_Component;

   begin
      if Length(Child.Name) > 0 then
         return Child_Parent.Name = Parent.Name;
      end if;

      return False;
   end Is_Child;

end Paths.String_Paths;
