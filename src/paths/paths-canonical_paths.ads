--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package Paths.Canonical_Paths is

   type Canonicalized_Path is new Canonical_Path_Type with private;


   -- Constructors --

   --
   -- Make_Path
   --
   --  Create a Path, given the vector of path components.
   --
   --  The Directory parameter is a Boolean which determines whether
   --  the created path should represent a directory (True) or a file
   --  (False).
   --
   function Make_Path (Components : Component_Vector; Directory : Boolean) return Canonicalized_Path;


   -- Conversions --

   function To_String (P : Canonicalized_Path) return Path_String;


   -- Path Components --

   function Components (P : Canonicalized_Path) return Component_Vector;
   function Basename (P : Canonicalized_Path) return Path_String;


   -- Path Predicates --

   function Is_Directory (P : Canonicalized_Path) return Boolean;
   function Has_Directories (P : Canonicalized_Path) return Boolean;

   function Is_Empty (P : Canonicalized_Path) return Boolean;
   function Is_Root (P : Canonicalized_Path) return Boolean;
   function Is_Relative (P : Canonicalized_Path) return Boolean;


   -- Operations on Paths --

   function Ensure_Directory (P : Canonicalized_Path; Directory : Boolean := True) return Canonicalized_Path;
   procedure Ensure_Directory (P : in out Canonicalized_Path; Directory : in Boolean := True);

   function Append (P1, P2 : Canonicalized_Path) return Canonicalized_Path;
   procedure Append (P1 : in out Canonicalized_Path; P2 : in Canonicalized_Path);

   function Remove_Last_Component (P : Canonicalized_Path) return Canonicalized_Path;
   procedure Remove_Last_Component (P : in out Canonicalized_Path);

   function Merge (P1, P2 : Canonicalized_Path) return Canonicalized_Path;
   procedure Merge (P1 : in out Canonicalized_Path; P2 : in Canonicalized_Path);

   function Canonicalize (P : Canonicalized_Path; Directory : Boolean := False) return Canonical_Path;


   -- Comparing Paths --

   function "=" (P1 : Canonicalized_Path; P2 : Canonicalized_Path) return Boolean;

   function Is_Subpath (Child : Canonicalized_Path; Parent : Canonicalized_Path; Check_Directory : Boolean := False) return Boolean;

   function Is_Child (Child : Canonicalized_Path; Parent : Canonicalized_Path) return Boolean;


private

   type Canonicalized_Path is new Canonical_Path_Type with record
      Path_Components : Component_Vector;
      Directory       : Boolean;
   end record;

end Paths.Canonical_Paths;
