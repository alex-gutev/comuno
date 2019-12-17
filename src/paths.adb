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
with Ada.Directories;

package body Paths is
   package Dirs renames Ada.Directories;

   -- Constructors --

   function Make_Path (Name : Path_String) return Path is
   begin
      return Path'(Name => To_Unbounded_String(Name));
   end Make_Path;

   function Make_Path (Name : Unbounded_Path_String) return Path is
   begin
      return Path'(Name => Name);
   end Make_Path;


   function Make_Path (Name : Path_String; Directory : Boolean) return Path is
      P : Path := Make_Path(Name);
   begin
      Ensure_Directory(P, Directory);
      return P;
   end Make_Path;

   function Make_Path (Name : Unbounded_Path_String; Directory : Boolean) return Path is
      P : Path := Make_Path(Name);
   begin
      Ensure_Directory(P, Directory);
      return P;
   end Make_Path;


   function Make_Path (Comps : Component_Array; Directory : Boolean := False) return Path is
      P : Path;
   begin
      for I in Comps'Range loop
         Append(P, Make_Path(Comps(I)));
      end loop;

      Ensure_Directory(P, Directory);

      return P;
   end Make_Path;

   function Make_Path (Comps : Component_Vector; Directory : Boolean := False) return Path is
      package Vec renames Component_Vectors;

      P : Path;

      procedure Add_Component (Comp_Cursor : Vec.Cursor) is
         Comp : Unbounded_Path_String := Vec.Element(Comp_Cursor);
      begin
         Append(P, Make_Path(Comp));
      end Add_Component;
   begin
      Comps.Iterate (Add_Component'Access);
      Ensure_Directory(P, Directory);

      return P;
   end Make_Path;


   -- Conversions --

   function To_String (P : Path) return Path_String is
   begin
      return To_String(P.Name);
   end To_String;


   -- Path Components --

   function Components (P : Path) return Component_Vector is
      package Vec renames Component_Vectors;

      Comps     : Component_Vector;
      Pos       : Positive      := 1;
      Next      : Natural;

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

   function Components (P : Path) return Component_Array is
      package Vec renames Component_Vectors;

      function To_Components_Array (Comps : Vec.Vector) return Component_Array is
         use type Ada.Containers.Count_Type;
         Arr : Component_Array(1..Natural(Comps.Length));
      begin
         for I in Arr'Range loop
            Arr(I) := Comps.Element(I);
         end loop;

         return Arr;
      end To_Components_Array;
   begin
      return To_Components_Array(Components(P));
   end Components;


   function Extension_Index (Path : Path_String) return Natural is
      Dot_Index : Natural := Index(Path, ".", Going => Backward);

   begin

      if Dot_Index > Path'First + 1 and Dot_Index /= Path'Last then
         return Dot_Index;
      else
         return 0;
      end if;

   end Extension_Index;



   function Basename (P : Path) return Path_String is
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

   function Filename (P : Path) return Path_String is
      Base      : Path_String   := Basename(P);
      Index     : Natural       := Extension_Index(Base);

   begin
      if Index = 0 then
         return Base;
      else
         return Base(Base'First .. Index - 1);
      end if;
   end Filename;

   function Extension (P : Path) return Path_String is
      Base      : Path_String   := Basename(P);
      Index     : Natural       := Extension_Index(Base);

   begin

      if Index = 0 then
         return "";
      else
         return Base(Index + 1 .. Base'Last);
      end if;

   end Extension;


   -- Path Predicates --

   function Is_Directory (P : Path) return Boolean is
   begin
      return Length(P.Name) > 0 and then
        Element(P.Name, Length(P.Name)) = Separator;
   end Is_Directory;

   function Has_Directories (P : Path) return Boolean is
   begin

      return Index(P.Name, Separator_String) > 0 or else
        P.Name = "." or else P.Name = ".." or else
        (Length(P.Name) > 0 and then Element(P.Name, 1) = '~');

   end Has_Directories;

   function Is_Empty (P : Path) return Boolean is
   begin
      return Length(P.Name) = 0;
   end Is_Empty;

   function Is_Root (P : Path) return Boolean is
      function Is_Root_Equivalent return Boolean is
      begin
         if Length(P.Name) > 0 then
            for I in 1 .. Length(P.Name) loop
               if Element(P.Name, I) /= '/' then
                  return False;
               end if;
            end loop;

            return True;

         else
            return False;
         end if;
      end Is_Root_Equivalent;


   begin
      return P.Name = "/" or else Is_Root_Equivalent;
   end Is_Root;

   function Is_Relative (P : Path) return Boolean is
   begin
      return Length(P.Name) = 0 or else
        (Element(P.Name, 1) /= Separator and Element(P.Name, 1) /= '~');
   end Is_Relative;


   -- Operations on Paths --

   function Ensure_Directory (P : Path; Directory : Boolean := True) return Path is
      Copy : Path := P;
   begin
      Ensure_Directory(Copy, Directory);
      return Copy;
   end Ensure_Directory;

   procedure Ensure_Directory (P : in out Path; Directory : Boolean := True) is
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


   function Append (P1 : Path; P2: Path) return Path is
      Copy : Path := P1;
   begin
      Append(Copy, P2);
      return Copy;
   end Append;

   procedure Append (P1 : in out Path; P2 : in Path) is
   begin
      Ensure_Directory(P1, True);

      Append(P1.Name, P2.Name);
   end Append;


   function Remove_Last_Component (P : Path) return path is
      Copy : Path := P;
   begin
      Remove_Last_Component(Copy);
      return Copy;
   end Remove_Last_Component;

   procedure Remove_Last_Component (P : in out Path) is
   begin
      if Length(P.Name) > 1 then
         declare
            Dir : Boolean := Is_Directory(P);
            Max : Positive := Length(P.Name) - (if Dir then 1 else 0);
            Pos : Natural := Index(P.Name, Separator_String, From => Max, Going => Backward);
         begin
            Head(P.Name, (if Pos > 1 and not Dir then Pos - 1 else Pos));
         end;
      else
         P.Name := Null_Unbounded_String;
      end if;
   end Remove_Last_Component;


   function Merge (P1 : Path; P2 : Path) return Path is
      Copy : Path := P1;
   begin
      Merge(Copy, P2);
      return Copy;
   end Merge;

   procedure Merge (P1 : in out Path; P2 : in Path) is
   begin
      if Is_Relative(P2) then
         if not Is_Directory(P1) then
            Remove_Last_Component(P1);
         end if;

         Append(P1, P2);
      else
         P1.Name := P2.Name;
      end if;
   end Merge;


   function Canonicalize (P : Path; Directory : Boolean := False) return Path is
      Copy : Path := P;
   begin
      Canonicalize(Copy, Directory);
      return Copy;
   end Canonicalize;

   procedure Canonicalize (P : in out Path; Directory : in Boolean := False) is
      package Vec renames Component_Vectors;

      New_Components : Component_Vector;
      Old_Components : Component_Vector := Components(P);

      procedure Add_Component (Comp_Cursor : Vec.Cursor) is
         use type Ada.Containers.Count_Type;
         Component : Unbounded_Path_String := Vec.Element(Comp_Cursor);
      begin
         if Component = ".." then
            -- If at least one component and last component is not
            -- '..'
            if New_Components.Length > 0 and then
              New_Components.Last_Element /= ".."

            then
               New_Components.Delete_Last;

            else
               New_Components.Append(Component);

            end if;

         elsif Component /= "." and Length(Component) > 0 then
            New_Components.Append(Component);

         end if;
      end Add_Component;

   begin
      Old_Components.Iterate(Add_Component'Access);
      P := Make_Path(New_Components);

      Ensure_Directory(P, Directory);
   end Canonicalize;


   -- Comparing Paths

   function "=" (P1 : Path; P2 : Path)  return Boolean is
   begin
      return P1.Name = P2.Name;
   end "=";

   function Is_Subpath (Child : Path; Parent : Path; Check_Directory : Boolean := False) return Boolean is
   begin
      if Length(Parent.Name) >= Length(Child.Name) then
         return False;

      elsif Length(Parent.Name) = 0 then
         return True;

      elsif Check_Directory and not Is_Directory(Parent) then
         return False;

      elsif Head(Child.Name, Length(Parent.Name)) = Parent.Name then
         return Is_Directory(Parent) or
           Element(Child.Name, Length(Parent.Name) + 1) = Separator;

      else
         return False;

      end if;
   end Is_Subpath;

   function Is_Child (Child : Path; Parent : Path) return Boolean is
      Child_Parent : Path := Remove_Last_Component(Child);
   begin
      if Length(Child.Name) > 0 then
	 return Child_Parent.Name = Parent.Name;
      end if;

      return False;
   end Is_Child;

end Paths;
