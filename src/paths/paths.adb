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

package body Paths is

   -- Path Components --

   function Components (P : Path_Type) return Component_Array is
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

   function Filename (P : Path_Type) return Path_String is
      Base  : Path_String := Basename(Path(P));
      Index : Natural     := Extension_Index(Base);

   begin
      if Index = 0 then
         return Base;
      else
         return Base(Base'First .. Index - 1);
      end if;
   end Filename;

   function Extension (P : Path_Type) return Path_String is
      Base  : Path_String := Basename(Path(P));
      Index : Natural     := Extension_Index(Base);

   begin
      if Index = 0 then
         return "";
      else
         return Base(Index + 1 .. Base'Last);
      end if;
   end Extension;

end Paths;
