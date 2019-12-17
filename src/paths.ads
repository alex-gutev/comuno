--  Copyright (C) 2019 Alexander Gutev <alex.gutev@mail.bg>
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

--
-- Purpose:
--
--  This package provides a Path data type and a set of operations on
--  Paths.
--
package Paths is
   --
   -- Fixed and unbounded string types, in which string
   -- representations of paths are stored.
   --
   subtype Path_String is String;
   subtype Unbounded_Path_String is Ada.Strings.Unbounded.Unbounded_String;

   --
   -- Instance of the Vectors package for vectors storing
   -- Unbounded_Path_String's.
   --
   use type Unbounded_Path_String;
   package Component_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_Path_String);

   --
   -- Vector and array of path components. Each component is
   -- represented as an Unbounded_Path_String.
   --
   subtype Component_Vector is Component_Vectors.Vector;
   type Component_Array is array (Natural range <>) of Unbounded_Path_String;

   type Path is private;

   --
   -- Path component separator character
   --
   Separator : constant Character := '/';
   Separator_String : constant Path_String(1..1) := (1 => Separator);


   -- Constructors --

   --
   -- Make_Path
   --
   --  Create a Path, given the path location string.
   --
   function Make_Path (Name : Path_String) return Path;
   function Make_Path (Name : Unbounded_Path_String) return Path;

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
   function Make_Path (Name : Path_String; Directory : Boolean) return Path;
   function Make_Path (Name : Unbounded_Path_String; Directory : Boolean) return Path;

   --
   -- Make_Path
   --
   --  Create a Path from an array/Vector of path component strings.
   --
   --  The Directory parameter is a Boolean which determines whether
   --  the created path should represent a directory (True) or a file
   --  (False).
   --
   function Make_Path (Comps : Component_Array; Directory : Boolean := False) return Path;
   function Make_Path (Comps : Component_Vector; Directory : Boolean := False) return Path;


   -- Conversions --

   --
   -- To_String
   --
   --  Returns a string representation of the path.
   --
   function To_String (P : Path) return Path_String;


   -- Path Components --

   --
   -- Components
   --
   --  Returns an array/Vector containing all the components of the
   --  path.
   --
   function Components (P : Path) return Component_Vector;
   function Components (P : Path) return Component_Array;

   --
   -- Basename
   --
   --  Returns the base component, i.e. the last component excluding
   --  the intermediate directories, of a path.
   --
   --  The base component of a path which represents a directory is
   --  the empty string.
   --
   function Basename (P : Path) return Path_String;

   --
   -- Filename
   --
   --  Returns the file name of the base component, excluding the
   --  extension.
   --
   --  The file name of a path which represents a directory is the
   --  empty string.
   --
   function Filename (P : Path) return Path_String;

   --
   -- Extension
   --
   --  Returns the file extension of the base component.
   --
   --  The extension of a path which represents a directory is the
   --  empty string.
   --
   function Extension (P : Path) return Path_String;


   -- Path Predicates --

   --
   -- Is_Directory
   --
   --  Returns true if path represents a directory.
   --
   -- Notes:
   --
   --  This only checks whether the path syntactically represents a
   --  directory, not whether the path actually points to a directory.
   --
   --  Returns False if the path syntactically appears as a file path,
   --  even if it is only valid if it is a directory path.
   --
   function Is_Directory (P : Path) return Boolean;

   --
   -- Has_Directories
   --
   --  Returns true if a path has at least one directory component.
   --
   -- Notes:
   --
   --  Returns False if the path syntactically appears as a file path,
   --  even if it is only valid if it is a directory path.
   --
   function Has_Directories (P : Path) return Boolean;

   --
   -- Is_Empty
   --
   --  Returns true if a path is empty, that is its string
   --  representation is the empty string.
   --
   function Is_Empty (P : Path) return Boolean;

   --
   -- Is_Root
   --
   --  Returns true if a path represents the file system root.
   --
   -- Notes:
   --
   --  A path consisting of the root path followed by the self
   --  directory component is not considered the file system root even
   --  though it points to the same directory.
   --
   function Is_Root (P : Path) return Boolean;

   --
   -- Is_Relative
   --
   --  Returns true if a path is a relative path.
   --
   -- Notes:
   --
   --  This returns false for paths beginning with a tilde.
   --
   function Is_Relative (P : Path) return Boolean;


   -- Operations on Paths --

   --
   -- Ensure_Directory
   --
   --  Change a path such that it either represents a directory
   --  (Directory = True) or a file (Directory = False).
   --
   --  The function returns a new path, whereas the procedure modifies
   --  an existing path.
   --
   -- Notes:
   --
   --  This function/procedure has no effect on the empty path, that
   --  is it does not produce a path which represents a directory.
   --
   function Ensure_Directory (P : Path; Directory : Boolean := True) return Path;
   procedure Ensure_Directory (P : in out Path; Directory : Boolean := True);

   --
   -- Append
   --
   --  Append a path (P2) to another path (P1), inserting a separator
   --  between the paths if necessary.
   --
   --  The function returns a new path, whereas the procedure modifies
   --  an existing path.
   --
   function Append (P1, P2 : Path) return Path;
   procedure Append (P1 : in out Path; P2 : in Path);

   --
   -- Remove_Last_Component
   --
   --  Remove the last component of a path. If the old path represents
   --  a directory, the new path also represents a directory.
   --
   --  The function returns a new path, whereas the procedure modifies
   --  an existing path.
   --
   -- Notes:
   --
   --  If the path consists of just the file system root component,
   --  the result is the empty path.
   --
   function Remove_Last_Component (P : Path) return Path;
   procedure Remove_Last_Component (P : in out Path);

   --
   -- Merge
   --
   --  Merge path P2 onto path P1.
   --
   --  If P2 is an absolute path, it replaces P1 otherwise this
   --  operation is identical to Append.
   --
   --  The function returns a new path, whereas the procedure modifies
   --  an existing path.
   --
   function Merge (P1, P2 : Path) return Path;
   procedure Merge (P1 : in out Path; P2 : in Path);

   --
   -- Canonicalize
   --
   --  Change a path to a canonical representation.
   --
   --  Self components (.), empty components and non-leading parent
   --  (..)  components are removed.
   --
   --  The function returns a new path, whereas the procedure modifies
   --  an existing path.
   --
   --  If Directory is True the resulting path represents a directory,
   --  otherwise if Directory is False, the resulting path represents
   --  a file.
   --
   function Canonicalize (P : Path; Directory : Boolean := False) return Path;
   procedure Canonicalize (P : in out Path; Directory : in Boolean := False);

   --  function Expand_Tilde (P : Path) return Path;


   -- Comparing Paths --

   --
   -- Return true if two paths are equal.
   --
   -- Paths are considered equal if their string representations are
   -- equal.
   --
   function "=" (P1 : Path; P2 : Path) return Boolean;

   --
   -- Is_Subpath
   --
   --   Return true if a path (Child) is a sub-path of another path
   --   (Parent).
   --
   --   A path (Child) is considered a sub-path of another path
   --   (Parent) if the path components of Parent are equal to the
   --   corresponding initial path components of Child.
   --
   --   If Check_Directory is true this function only returns true if
   --   Parent represents a directory.
   --
   function Is_Subpath (Child : Path; Parent : Path; Check_Directory : Boolean := False) return Boolean;

   --
   -- Is_Child
   --
   --  Return true if a path (Child) is an immediate child of a parent
   --  path (Parent).
   --
   --  A path (Child) is considered a child of another path (Parent)
   --  if Child contains one more component than Parent and each
   --  component of Child, excluding the last, is equal to the
   --  corresponding component of Parent.
   --
   function Is_Child (Child : Path; Parent : Path) return Boolean;


private

   type Path is record
      Name : Unbounded_Path_String;
   end record;

end Paths;
