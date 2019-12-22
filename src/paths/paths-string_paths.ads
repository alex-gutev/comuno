--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package Paths.String_Paths is

   type String_Path is new Path_Type with private;


   -- Constructors --

   --
   -- Make_Path
   --
   --  Create a Path, given the path's location string.
   --
   function Make_Path (Name : Path_String) return String_Path;
   function Make_Path (Name : Unbounded_Path_String) return String_Path;

   --
   -- Make_Path
   --
   --  Create a Path, given the path's location string.
   --
   --  The Directory parameter is a Boolean which determines whether
   --  the created path should represent a directory (True) or a file
   --  (False).
   --
   -- Notes:
   --
   --  If Name represents the file system root, the resulting path
   --  represents a directory even if Directory is False.
   --
   --  If Name is the empty string, the resulting path is the empty
   --  path, which does not represent a directory, even if Directory
   --  is True.
   --
   function Make_Path (Name : Path_String; Directory : Boolean) return String_Path;
   function Make_Path (Name : Unbounded_Path_String; Directory : Boolean) return String_Path;

   --
   -- Make_Path
   --
   --  Create a Path from an array/Vector of path component strings.
   --
   --  The Directory parameter is a Boolean which determines whether
   --  the created path should represent a directory (True) or a file
   --  (False).
   --
   function Make_Path (Components : Component_Array; Directory : Boolean := False) return String_Path;
   function Make_Path (Components : Component_Vector; Directory : Boolean := False) return String_Path;


   -- Conversions --

   function To_String (P : String_Path) return Path_String;


   -- Path Components --

   function Components (P : String_Path) return Component_Vector;
   function Basename (P : String_Path) return Path_String;


   -- Path Predicates --

   function Is_Directory (P : String_Path) return Boolean;
   function Has_Directories (P : String_Path) return Boolean;

   function Is_Empty (P : String_Path) return Boolean;
   function Is_Root (P : String_Path) return Boolean;
   function Is_Relative (P : String_Path) return Boolean;


   -- Operations on Paths --

   function Ensure_Directory (P : String_Path; Directory : Boolean := True) return String_Path;
   procedure Ensure_Directory (P : in out String_Path; Directory : in Boolean := True);

   function Append (P1, P2 : String_Path) return String_Path;
   procedure Append (P1 : in out String_Path; P2 : in String_Path);

   function Remove_Last_Component (P : String_Path) return String_Path;
   procedure Remove_Last_Component (P : in out String_Path);

   function Merge (P1, P2 : String_Path) return String_Path;
   procedure Merge (P1 : in out String_Path; P2 : in String_Path);

   function Canonicalize (P : String_Path; Directory : Boolean := False) return Canonical_Path;


   -- Comparing Paths --

   function "=" (P1 : String_Path; P2 : String_Path) return Boolean;

   function Is_Subpath (Child : String_Path; Parent : String_Path; Check_Directory : Boolean := False) return Boolean;

   function Is_Child (Child : String_Path; Parent : String_Path) return Boolean;


private

   type String_Path is new Path_Type with record
      Name : Unbounded_Path_String;
   end record;

end Paths.String_Paths;
