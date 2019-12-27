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

package body Paths is

   -- Constructors --

   function Make_Path (Name : Path_String) return Path is
     (Name => To_Unbounded_String(Name));

   function Make_Path (Name : Unbounded_Path_String) return Path is
     (Name => Name);


   function Make_Path (Name : Path_String; Directory : Boolean) return Path is
      P : Path := Make_Path(Name);

   begin
      P.Ensure_Directory(Directory);
      return P;
   end Make_Path;


   function Make_Path (Name : Unbounded_Path_String; Directory : Boolean) return Path is
      P : Path := Make_Path(Name);

   begin
      P.Ensure_Directory(Directory);
      return P;
   end Make_Path;


   function Make_Path (Components : Component_Array; Directory : Boolean := False) return Path is
      P : Path;

   begin
      for I in Components'Range loop
         P.Append(Make_Path(Components(I)));
      end loop;

      P.Ensure_Directory(Directory);

      return P;
   end Make_Path;

   function Make_Path (Components : Component_Vector; Directory : Boolean := False) return Path is
      P : Path;

   begin
      for Component of Components loop
         P.Append(Make_Path(Component));
      end loop;

      P.Ensure_Directory(Directory);

      return P;
   end Make_Path;


   -- Conversions --

   function To_String (P : Path) return Path_String is
     (To_String(P.Name));


   -- Path Components --

   function Components (P : Path) return Component_Vector is
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

   function Components (P : Path) return Component_Array is
      package Vec renames Component_Vectors;

      function To_Components_Array (Components : Vec.Vector) return Component_Array is
         use type Ada.Containers.Count_Type;
         Arr : Component_Array(Components.First_Index .. Natural(Components.Length));
      begin
         for I in Arr'Range loop
            Arr(I) := Components(I);
         end loop;

         return Arr;
      end To_Components_Array;
   begin
      return To_Components_Array(Components(Path(P)));
   end Components;


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

   --
   -- Extension_Index
   --
   --  Returns the index of the character separating the filename from
   --  the extension. Returns 0 if there is no extension.
   --
   function Extension_Index (Path : Path_String) return Natural is
      Dot_Index : Natural := Index(Path, ".", Going => Backward);

   begin
      if Dot_Index > Path'First + 1 and Dot_Index /= Path'Last then
         return Dot_Index;
      else
         return 0;
      end if;
   end Extension_Index;

   function Filename (P : Path) return Path_String is
      Base  : Path_String := Basename(Path(P));
      Index : Natural     := Extension_Index(Base);

   begin
      if Index = 0 then
         return Base;
      else
         return Base(Base'First .. Index - 1);
      end if;
   end Filename;

   function Extension (P : Path) return Path_String is
      Base  : Path_String := Basename(Path(P));
      Index : Natural     := Extension_Index(Base);

   begin
      if Index = 0 then
         return "";
      else
         return Base(Index + 1 .. Base'Last);
      end if;
   end Extension;


   -- Path Predicates --

   function Is_Directory (P : Path) return Boolean is
     (Length(P.Name) > 0 and then
        Element(P.Name, Length(P.Name)) = Separator);

   function Has_Directories (P : Path) return Boolean is
     (Index(P.Name, Separator_String) > 0 or else
        P.Name = Self_Component or else P.Name = Parent_Component or else
        (Length(P.Name) > 0 and then Element(P.Name, 1) = '~'));

   function Is_Empty (P : Path) return Boolean is
     (Length(P.Name) = 0);

   function Is_Root (P : Path) return Boolean is
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

   function Is_Relative (P : Path) return Boolean is
     (Length(P.Name) = 0 or else
        (Element(P.Name, 1) /= Separator and Element(P.Name, 1) /= '~'));


   -- Operations on Paths --

   function Ensure_Directory (P : Path; Directory : Boolean := True) return Path is
      Copy : Path := P;
   begin
      Copy.Ensure_Directory(Directory);
      return Copy;
   end Ensure_Directory;

   procedure Ensure_Directory (P : in out Path; Directory : in Boolean := True) is
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
      Copy.Append(P2);
      return Copy;
   end Append;

   procedure Append (P1 : in out Path; P2 : in Path) is
   begin
      P1.Ensure_Directory(True);

      Append(P1.Name, P2.Name);
   end Append;


   function Remove_Last_Component (P : Path) return path is
      Copy : Path := P;

   begin
      Copy.Remove_Last_Component;
      return Copy;
   end Remove_Last_Component;

   procedure Remove_Last_Component (P : in out Path) is
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


   function Merge (P1 : Path; P2 : Path) return Path is
      Copy : Path := P1;

   begin
      Copy.Merge(P2);
      return Copy;
   end Merge;

   procedure Merge (P1 : in out Path; P2 : in Path) is
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


   -- Comparing Paths

   function "=" (P1 : Path; P2 : Path)  return Boolean is
     (P1.Name = P2.Name);

   function Is_Subpath (Child : Path; Parent : Path; Check_Directory : Boolean := False) return Boolean is
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

   function Is_Child (Child : Path; Parent : Path) return Boolean is
      Child_Parent : Path := Child.Remove_Last_Component;

   begin
      if Length(Child.Name) > 0 then
         return Child_Parent.Name = Parent.Name;
      end if;

      return False;
   end Is_Child;

end Paths;
