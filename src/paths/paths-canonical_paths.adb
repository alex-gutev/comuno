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

package body Paths.Canonical_Paths is

   function Canonicalize (P : Path'Class; Directory : Boolean := False) return Canonical_Path is
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

      return Make_Path(New_Components, Directory);
   end Canonicalize;


   -- Operations on Paths --

   function Append (P1, P2 : Canonical_Path) return Canonical_Path is
      Copy : Canonical_Path := P1;
   begin
      Copy.Append(P2);
      return Copy;
   end Append;

   procedure Append (P1 : in out Canonical_Path; P2 : in Canonical_Path) is
   begin
      Append(Path(P1), Path(P2));
      P1 := Canonicalize(P1, P1.Is_Directory);
   end Append;


   function Merge (P1, P2 : Canonical_Path) return Canonical_Path is
      Copy : Canonical_Path := P1;
   begin
      Copy.Merge(P2);
      return Copy;
   end Merge;

   procedure Merge (P1 : in out Canonical_Path; P2 : in Canonical_Path) is
   begin
      Merge(Path(P1), Path(P2));
      P1 := Canonicalize(P1, P1.Is_Directory);
   end Merge;


end Paths.Canonical_Paths;
